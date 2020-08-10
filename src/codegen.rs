//! Generates Rust code from IR.

#![cfg_attr(rustfmt, rustfmt::skip::macros(write_gen))]

use crate::ir::cc::{CcIr, RsIr};
use crate::ir::{bindings, cc, rs};
use gen_macro::{snippet, write_gen, Gen, Snippet};
use itertools::Itertools;
use std::io::{self, Write};

pub(crate) use gen_macro::CodeWriter;

pub(crate) struct Outputs<'a> {
    pub rs: Option<CodeWriter<'a>>,
    pub cc: Option<CodeWriter<'a>>,
    pub hdr: Option<CodeWriter<'a>>,
}

#[rustfmt::skip::macros(write_gen)]
pub(crate) fn perform_codegen(
    db: &(impl RsIr + CcIr),
    mdl: &rs::BindingsCrate,
    headers: &[bindings::Header],
    skip_header: bool,
    mut out: Outputs<'_>,
) -> io::Result<()> {
    let out = &mut out;

    if !skip_header {
        if let Some(rs) = out.rs.as_mut() {
            write_gen!(db, rs, "
                #![allow(non_camel_case_types)]
                extern crate core;

            ")?;
        }
        if let Some(cc) = out.cc.as_mut() {
            for hdr in headers {
                let include_path = match hdr.is_system {
                    true => Snippet::from(format!(r#""{}""#, hdr.path)),
                    false => Snippet::from(format!(r#"<{}>"#, hdr.path)),
                };
                write_gen!(db, cc, r#"
                    #include $include_path

                "#)?;
            }
        }
    }

    for item in &mdl.items {
        match item {
            rs::ItemKind::Struct(st) => gen_struct(db, &st.lookup(db), out)?,
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

#[rustfmt::skip::macros(write_gen)]
fn gen_struct(db: &(impl RsIr + CcIr), st: &rs::Struct, out: &mut Outputs<'_>) -> io::Result<()> {
    if let Some(rs) = out.rs.as_mut() {
        let rs::Struct {
            vis, name, align, ..
        } = st;
        write_gen!(db, rs, "
            #[repr(C, align($align))]
            ${vis}struct $name {
        ")?;
        rs.with_indent(|rs| -> io::Result<()> {
            for field in &st.fields {
                let rs::Field { vis, name, .. } = field;
                let ty = field.ty(db);
                write_gen!(db, rs, "
                    ${vis}$name: $ty,
                ")?;
            }
            Ok(())
        })?;
        write_gen!(db, rs, "
            }
        ")?;
    }

    for method in &st.methods {
        gen_method(db, st, method, out)?;
    }

    Ok(())
}

#[rustfmt::skip::macros(write_gen)]
fn gen_method(
    db: &(impl RsIr + CcIr),
    st: &rs::Struct,
    meth: &rs::Method,
    out: &mut Outputs<'_>,
) -> io::Result<()> {
    // TODO handle visibility (don't generate bindings for private methods)
    // TODO handle constness
    let func = meth.func();
    debug_assert_eq!(func.param_tys.len(), func.param_names.len());
    assert!(func.is_method, "static methods aren't supported yet");

    let mangled_path = st.name.to_string().replace("::", "__");
    let c_func_name: Snippet = format!("_bind_{}__{}", mangled_path, func.name).into();

    if let Some(rs) = out.rs.as_mut() {
        let arg_names = arg_names(meth);
        let args_sig: Snippet = arg_names
            .iter()
            .zip(meth.param_tys(db))
            .map(|(name, ty)| snippet!(db, "$name: $ty").to_string())
            .join(", ")
            .into();
        let arg_names: Snippet = arg_names.iter().join(", ").into();
        let ret_ty = meth.return_ty(db);

        let func_name = &func.name;
        let struct_name = &st.name;
        let trait_name = snippet!(db, "${struct_name}_${func_name}_Ext");

        // Create an extension trait for our method.
        write_gen!(db, rs, "
            pub trait $trait_name {
                fn $func_name(self, $args_sig) -> $ret_ty;
            }
        ")?;

        // impl the extension trait for NonNull<Struct>.
        write_gen!(db, rs, r#"
            impl $trait_name for ::core::ptr::NonNull<$struct_name> {
                fn $func_name(self, $args_sig) -> $ret_ty {
                    extern "C" { fn $c_func_name(this: *mut $struct_name, $args_sig) -> $ret_ty; }
                    unsafe { $c_func_name(self.as_ptr(), $arg_names) }
                }
            }
        "#)?;

        // Create a convenience wrapper for &mut self.
        write_gen!(db, rs, "
            impl $struct_name {
                pub fn $func_name(&mut self, $args_sig) -> $ret_ty {
                    ::core::ptr::NonNull::from(self).$func_name($arg_names)
                }
            }
        ")?;
    }

    if let Some(cc) = out.cc.as_mut() {
        let func = meth.cc_func(db);
        let arg_names = arg_names(meth);
        let args_sig: Snippet = arg_names
            .iter()
            .zip(func.param_tys(db))
            .map(|(name, ty)| snippet!(db, "$ty $name").to_string())
            .join(", ")
            .into();
        let arg_names: Snippet = arg_names.iter().join(", ").into();
        let cc_st = st.cc_id.lookup(db);
        let st_name = cc_st.name;
        let func_name = &func.name;
        let ret_ty = func.return_ty(db);
        write_gen!(db, cc, r#"
            extern "C" $ret_ty $c_func_name($st_name* self, $args_sig) {
                return self->$st_name::$func_name($arg_names);
            }
        "#)?;
    }

    Ok(())
}

fn arg_names(meth: &rs::Method) -> Vec<rs::Ident> {
    meth.func()
        .param_names
        .iter()
        .enumerate()
        .map(|(idx, name)| name.clone().unwrap_or_else(|| format!("_{}__", idx).into()))
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

macro_rules! impl_gen_from_display {
    ($ty:path) => {
        impl<DB> Gen<DB> for $ty {
            fn gen(&self, _db: &DB, f: &mut CodeWriter<'_>) -> io::Result<()> {
                write!(f, "{}", self)
            }
        }
    };
}

impl_gen_from_display!(rs::Align);
impl_gen_from_display!(rs::Ident);
impl_gen_from_display!(rs::Path);

impl<DB: RsIr> Gen<DB> for rs::Ty {
    fn gen(&self, db: &DB, f: &mut CodeWriter<'_>) -> io::Result<()> {
        use rs::Ty::*;
        let name = match self {
            Error => "{error}",
            Unit => "()",
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

impl<DB: CcIr> Gen<DB> for cc::Ty {
    fn gen(&self, db: &DB, f: &mut CodeWriter<'_>) -> io::Result<()> {
        use cc::Ty::*;
        let name = match self {
            Error => "<error>",
            Void => "void",
            Short => "short",
            UShort => "unsigned short",
            Int => "int",
            UInt => "unsigned int",
            Long => "long",
            ULong => "unsigned long",
            LongLong => "long long",
            ULongLong => "unsigned long long",
            CharS => "char",
            CharU => "char",
            SChar => "signed char",
            UChar => "unsigned char",
            Size => "::std::size_t",
            SSize => "::std::ssize_t",
            PtrDiff => "::std::ptrdiff_t",
            Float => "float",
            Double => "double",
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
            pub trait Foo_sum_Ext {
                fn sum(self, c: i32, _1__: i32) -> i32;
            }
            impl Foo_sum_Ext for ::core::ptr::NonNull<Foo> {
                fn sum(self, c: i32, _1__: i32) -> i32 {
                    extern "C" { fn _bind_Foo__sum(this: *mut Foo, c: i32, _1__: i32) -> i32; }
                    unsafe { _bind_Foo__sum(self.as_ptr(), c, _1__) }
                }
            }
            impl Foo {
                pub fn sum(&mut self, c: i32, _1__: i32) -> i32 {
                    ::core::ptr::NonNull::from(self).sum(c, _1__)
                }
            }
        "#, r#"
            extern "C" int _bind_Foo__sum(Foo* self, int c, int _1__) {
                return self->Foo::sum(c, _1__);
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
