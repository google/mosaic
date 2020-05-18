//! Generates Rust code from IR.

#![cfg_attr(rustfmt, rustfmt::skip::macros(write_gen))]

use crate::ir::cc::RsIr;
use crate::ir::rs;
use gen_macro::{snippet, write_gen, Gen, Snippet};
use itertools::Itertools;
use proc_macro2::{Ident, Punct, Spacing, Span, TokenStream};
use quote::{ToTokens, TokenStreamExt};
use std::{
    borrow::Borrow,
    fmt,
    io::{self, Write},
};

pub fn perform_codegen(
    db: &impl RsIr,
    mdl: &rs::Module,
    out: &mut impl io::Write,
) -> io::Result<()> {
    let mut out = CodeWriter::new(out);
    for item in &mdl.items {
        match item {
            rs::ItemKind::Struct(st) => st.lookup(db).codegen(db, &mut out)?,
        }
    }
    Ok(())
}

struct CodeWriter<'a> {
    inner: &'a mut dyn io::Write,
    indent: u16,
    newline: bool,
}
impl<'a> CodeWriter<'a> {
    fn new(out: &'a mut impl io::Write) -> Self {
        CodeWriter {
            inner: out,
            indent: 0,
            newline: true,
        }
    }
    fn with_indent<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.indent += 1;
        let result = f(self);
        self.indent -= 1;
        result
    }
}
impl io::Write for CodeWriter<'_> {
    fn write(&mut self, mut buf: &[u8]) -> io::Result<usize> {
        let len = buf.len();
        //let mut unindented = unindent_bytes(buf);
        //dbg!(String::from_utf8_lossy(buf));
        //dbg!(String::from_utf8_lossy(&unindented));
        //if let Some(b'\n') = buf.last() {
        //    unindented.push(b'\n');
        //}
        //let mut buf: &[u8] = &unindented;
        while !buf.is_empty() {
            if self.newline {
                for _ in 0..self.indent {
                    self.inner.write_all(b"    ")?;
                }
            }
            match buf.iter().position(|b| *b == b'\n') {
                Some(i) => {
                    let next_line = i + 1;
                    self.inner.write_all(&buf[..next_line])?;
                    buf = &buf[next_line..];
                    self.newline = true;
                }
                None => {
                    self.inner.write_all(buf)?;
                    buf = &[];
                    self.newline = false;
                }
            }
        }
        Ok(len)
    }
    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

trait Codegen {
    fn codegen(&self, db: &impl RsIr, f: &mut CodeWriter) -> io::Result<()>;
}

impl Codegen for rs::Visibility {
    fn codegen(&self, _db: &impl RsIr, f: &mut CodeWriter) -> io::Result<()> {
        Ok(match self {
            rs::Visibility::Public => write!(f, "pub ")?,
            rs::Visibility::Private => (),
        })
    }
}

impl Codegen for rs::Struct {
    fn codegen(&self, db: &impl RsIr, f: &mut CodeWriter) -> io::Result<()> {
        writeln!(f, "#[repr(C, align({}))]", self.align)?;
        self.vis.codegen(db, f)?;
        writeln!(f, "struct {} {{", self.name)?;
        f.with_indent(|f| -> io::Result<()> {
            for field in &self.fields {
                field.vis.codegen(db, f)?;
                write!(f, "{}: ", field.name)?;
                field.ty(db).codegen(db, f)?;
                writeln!(f, ",")?;
            }
            Ok(())
        })?;
        writeln!(f, "}}")?;

        for method in &self.methods {
            gen_method(db, f, self, method)?;
        }

        Ok(())
    }
}

#[rustfmt::skip::macros(write_gen)]
fn gen_method(
    db: &impl RsIr,
    f: &mut CodeWriter,
    st: &rs::Struct,
    meth: &rs::Method,
) -> io::Result<()> {
    // TODO handle visibility (don't generate bindings for private methods)
    // TODO handle constness
    let func = meth.func();
    debug_assert_eq!(func.param_tys.len(), func.param_names.len());

    let arg_names = arg_names(meth);
    let args_sig: Snippet = arg_names
        .iter()
        .zip(meth.param_tys(db).with_db(db))
        .map(|(name, ty)| format!("{}: {}", name, ty))
        .join(", ")
        .into();

    let func_name = &func.name;
    let struct_name = &st.name;
    let trait_name = snippet!(db, "${struct_name}_${func_name}_Ext")?;

    // Create an extension trait for our method.
    write_gen!(f, db, "
        trait $trait_name {
            fn $func_name(self, $args_sig);
        }
    ")?;

    // impl the extension trait for NonNull<Struct>.
    write_gen!(f, db, "
        impl $trait_name for ::core::ptr::NonNull<$struct_name> {
            fn $func_name(self, $args_sig) {
                todo!()
            }
        }
    ")?;

    // Create a convenience wrapper for &mut self.
    let arg_names: Snippet = arg_names.iter().join(", ").into();
    write_gen!(f, db, "
        impl $struct_name {
            fn $func_name(&mut self, $args_sig) {
                ::core::ptr::NonNull::from(self).$func_name($arg_names)
            }
        }
    ")?;

    Ok(())
}

fn arg_names(meth: &rs::Method) -> Vec<rs::Ident> {
    meth.func()
        .param_names
        .iter()
        .enumerate()
        .map(|(idx, name)| name.clone().unwrap_or_else(|| format!("__{}", idx).into()))
        .collect()
}

/// Convenience trait to hide lifetime param.
trait DbRef: Copy {
    type DB: RsIr;
    fn get(&self) -> &Self::DB;
}
impl<DB: RsIr> DbRef for &'_ DB {
    type DB = DB;
    fn get(&self) -> &DB {
        &*self
    }
}

/// Used to attach a database ref to a value, so it can be used inside `quote!`.
trait WithDB: Sized {
    fn with_db<DB: DbRef>(self, db: DB) -> Context<DB, Self>;
}
impl<T> WithDB for T {
    fn with_db<DB: DbRef>(self, db: DB) -> Context<DB, Self> {
        Context { db, inner: self }
    }
}

struct Context<DB: DbRef, T> {
    db: DB,
    inner: T,
}

impl<DB: DbRef, T> Iterator for Context<DB, T>
where
    T: Iterator,
{
    type Item = Context<DB, T::Item>;
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|v| v.with_db(self.db))
    }
}

trait ToTokensDb {
    fn to_tokens_db(&self, db: &impl RsIr, ts: &mut TokenStream);
}
impl<T: ToTokensDb> ToTokensDb for &'_ T {
    fn to_tokens_db(&self, db: &impl RsIr, ts: &mut TokenStream) {
        (*self).to_tokens_db(db, ts)
    }
}

impl<'a, DB: DbRef, T: ToTokensDb> ToTokens for Context<DB, T> {
    fn to_tokens(&self, ts: &mut TokenStream) {
        self.inner.borrow().to_tokens_db(self.db.get(), ts)
    }
}
impl<'a, DB: DbRef, T: ToTokensDb> fmt::Display for Context<DB, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut ts = TokenStream::new();
        self.inner.borrow().to_tokens_db(self.db.get(), &mut ts);
        write!(f, "{}", ts)
    }
}

struct DoubleColon;
impl ToTokens for DoubleColon {
    fn to_tokens(&self, ts: &mut TokenStream) {
        ts.append(Punct::new(':', Spacing::Joint));
        ts.append(Punct::new(':', Spacing::Alone));
    }
}

impl ToTokens for rs::Ident {
    fn to_tokens(&self, ts: &mut TokenStream) {
        ts.append(Ident::new(self.as_str(), Span::call_site()));
    }
}
impl ToTokens for rs::Path {
    fn to_tokens(&self, ts: &mut TokenStream) {
        ts.append_separated(self.iter(), DoubleColon)
    }
}

impl<DB> Gen<DB> for rs::Ident {
    fn gen(&self, _db: &DB, f: &mut impl io::Write) -> io::Result<()> {
        write!(f, "{}", self.as_str())
    }
}
impl<DB> Gen<DB> for rs::Path {
    fn gen(&self, _db: &DB, f: &mut impl io::Write) -> io::Result<()> {
        for token in self.iter().map(rs::Ident::as_str).intersperse("::") {
            write!(f, "{}", token)?;
        }
        Ok(())
    }
}

impl ToTokensDb for rs::Ty {
    fn to_tokens_db(&self, db: &impl RsIr, ts: &mut TokenStream) {
        use rs::Ty::*;
        let name = match self {
            Error => "<error>",
            U8 => "u8",
            I8 => "i8",
            U16 => "u16",
            I16 => "i16",
            U32 => "u32",
            I32 => "i32",
            U64 => "u64",
            I64 => "i64",
            USize => "usize",
            ISize => "isize",
            F32 => "f32",
            F64 => "f64",
            Bool => "bool",
            Struct(id) => return id.lookup(db).name.to_tokens(ts),
        };
        ts.append(Ident::new(name, Span::call_site()));
    }
}
impl ToTokensDb for rs::Function {
    fn to_tokens_db(&self, _db: &impl RsIr, _ts: &mut TokenStream) {
        todo!()
    }
}

impl Codegen for rs::Ty {
    fn codegen(&self, db: &impl RsIr, f: &mut CodeWriter) -> io::Result<()> {
        let mut ts = TokenStream::new();
        self.with_db(db).to_tokens(&mut ts);
        write!(f, "{}", ts)
    }
}

impl<DB: RsIr> Gen<DB> for rs::Ty {
    fn gen(&self, db: &DB, f: &mut impl io::Write) -> io::Result<()> {
        let mut ts = TokenStream::new();
        self.with_db(db).to_tokens(&mut ts);
        write!(f, "{}", ts)
    }
}

#[cfg(test)]
mod tests {
    use crate::Session;

    #[test]
    fn pod() {
        let mut sess = Session::new();
        cpp_to_rs!(sess, {
            struct Pod {
                int a, b;
                char e;
                double c, d;
            };

            namespace rust_export {
              using ::Pod;
            }
        } => r#"
            #[repr(C, align(8))]
            pub struct Pod {
                pub a: i32,
                pub b: i32,
                pub e: i8,
                pub c: f64,
                pub d: f64,
            }
        "#);
    }

    #[test]
    fn nested_struct() {
        let mut sess = Session::new();
        cpp_to_rs!(sess, {
            struct Foo {
                int a, b;
            };
            struct Bar {
                char c, d;
                Foo foo;
            };
            namespace rust_export {
                using ::Bar;
            }
        } => r#"
            #[repr(C, align(4))]
            pub struct Bar {
                pub c: i8,
                pub d: i8,
                foo: Foo,
            }
            #[repr(C, align(4))]
            struct Foo {
                pub a: i32,
                pub b: i32,
            }
        "#);
    }

    #[test]
    fn method() {
        let mut sess = Session::new();
        cpp_to_rs!(sess, {
            struct Foo {
                int a, b;
                int sum(int c, int) const;
            };
            namespace rust_export {
                using ::Foo;
            }
        } => r#"
        #[repr(C, align(4))]
        pub struct Foo {
            pub a: i32,
            pub b: i32,
        }
        trait Foo_sum_Ext {
            fn sum(self, c: i32, __1: i32);
        }
        impl Foo_sum_Ext for ::core::ptr::NonNull<Foo> {
            fn sum(self, c: i32, __1: i32) {
                todo!()
            }
        }
        impl Foo {
            fn sum(&mut self, c: i32, __1: i32) {
                ::core::ptr::NonNull::from(self).sum(c, __1)
            }
        }
        "#);
    }

    // TODO handle these.
    #[test]
    #[should_panic(expected = "unsupported type")]
    fn nested_struct_in_namespace() {
        let mut sess = Session::new();
        cpp_to_rs!(sess, {
            namespace ns {
                struct Foo {
                    int a, b;
                };
            }
            struct Bar {
                char c, d;
                ns::Foo foo;
            };
            namespace rust_export {
                using ::Bar;
            }
        } => r#"
            #[repr(C, align(4))]
            struct Foo {
                pub a: i32,
                pub b: i32,
            }
            #[repr(C, align(4))]
            pub struct Bar {
                pub c: i8,
                pub d: i8,
                foo: Foo,
            }
        "#);
    }

    #[test]
    fn export_from_namespace() {
        let mut sess = Session::new();
        cpp_to_rs!(sess, {
            namespace ns {
                struct Foo {
                    int a, b;
                };
            }
            namespace rust_export {
                using ::ns::Foo;
            }
        } => r#"
            #[repr(C, align(4))]
            pub struct Foo {
                pub a: i32,
                pub b: i32,
            }
        "#);
    }

    #[test]
    fn namespace_in_export() {
        // Not supported yet.
        let mut sess = Session::new();
        cpp_lower!(sess, {
            struct Foo {
                int a, b;
            };
            namespace rust_export {
                namespace ns {
                    using ::Foo;
                }
            }
        } => [
            "invalid rust_export item"
        ]);
    }
}
