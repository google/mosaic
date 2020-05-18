//! Generates Rust code from IR.

#![cfg_attr(rustfmt, rustfmt::skip::macros(write_gen))]

use crate::ir::cc::RsIr;
use crate::ir::rs;
use gen_macro::{snippet, write_gen, CodeWriter, Gen, Snippet};
use itertools::Itertools;
use std::io::{self, Write};

pub fn perform_codegen(
    db: &impl RsIr,
    mdl: &rs::Module,
    out: &mut impl io::Write,
) -> io::Result<()> {
    let mut out = CodeWriter::new(out);
    for item in &mdl.items {
        match item {
            rs::ItemKind::Struct(st) => st.lookup(db).gen(db, &mut out)?,
        }
    }
    Ok(())
}

impl<DB: RsIr> Gen<DB> for rs::Visibility {
    fn gen(&self, _db: &DB, f: &mut CodeWriter) -> io::Result<()> {
        Ok(match self {
            rs::Visibility::Public => write!(f, "pub ")?,
            rs::Visibility::Private => (),
        })
    }
}

impl<DB: RsIr> Gen<DB> for rs::Struct {
    fn gen(&self, db: &DB, f: &mut CodeWriter) -> io::Result<()> {
        writeln!(f, "#[repr(C, align({}))]", self.align)?;
        self.vis.gen(db, f)?;
        writeln!(f, "struct {} {{", self.name)?;
        f.with_indent(|f| -> io::Result<()> {
            for field in &self.fields {
                field.vis.gen(db, f)?;
                write!(f, "{}: ", field.name)?;
                field.ty(db).gen(db, f)?;
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
        .zip(meth.param_tys(db))
        .map(|(name, ty)| snippet!(db, "$name: $ty").to_string())
        .join(", ")
        .into();

    let func_name = &func.name;
    let struct_name = &st.name;
    let trait_name = snippet!(db, "${struct_name}_${func_name}_Ext");

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

impl<DB> Gen<DB> for rs::Ident {
    fn gen(&self, _db: &DB, f: &mut CodeWriter<'_>) -> io::Result<()> {
        write!(f, "{}", self.as_str())
    }
}
impl<DB> Gen<DB> for rs::Path {
    fn gen(&self, _db: &DB, f: &mut CodeWriter<'_>) -> io::Result<()> {
        for token in self.iter().map(rs::Ident::as_str).intersperse("::") {
            write!(f, "{}", token)?;
        }
        Ok(())
    }
}

impl<DB: RsIr> Gen<DB> for rs::Ty {
    fn gen(&self, db: &DB, f: &mut CodeWriter<'_>) -> io::Result<()> {
        use rs::Ty::*;
        let name = match self {
            Error => "{error}",
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
            Struct(id) => return id.lookup(db).name.gen(db, f),
        };
        write!(f, "{}", name)
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
