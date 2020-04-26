//! Generates Rust code from IR.

use crate::ir::cc::RsIr;
use crate::ir::rs;
use std::io;

pub fn perform_codegen(
    db: &impl RsIr,
    mdl: &rs::Module,
    out: &mut impl io::Write,
) -> io::Result<()> {
    for item in &mdl.items {
        match item {
            rs::ItemKind::Struct(st) => st.lookup(db).gen(db, out)?,
        }
    }
    Ok(())
}

trait Codegen {
    fn gen(&self, db: &impl RsIr, f: &mut impl io::Write) -> io::Result<()>;
}

impl Codegen for rs::Visibility {
    fn gen(&self, _db: &impl RsIr, f: &mut impl io::Write) -> io::Result<()> {
        Ok(match self {
            rs::Visibility::Public => write!(f, "pub ")?,
            rs::Visibility::Private => (),
        })
    }
}

impl Codegen for rs::Struct {
    fn gen(&self, db: &impl RsIr, f: &mut impl io::Write) -> io::Result<()> {
        writeln!(f, "#[repr(C, align({}))]", self.align)?;
        self.vis.gen(db, f)?;
        writeln!(f, "struct {} {{", self.name)?;
        for field in &self.fields {
            write!(f, "    ")?;
            field.vis.gen(db, f)?;
            write!(f, "{}: ", field.name)?;
            field.ty(db).gen(db, f)?;
            writeln!(f, ",")?;
        }
        writeln!(f, "}}")?;

        for method in &self.methods {
            gen_method(db, f, self, method)?;
        }

        Ok(())
    }
}

fn gen_method<W: io::Write>(
    db: &impl RsIr,
    f: &mut W,
    st: &rs::Struct,
    meth: &rs::Method,
) -> io::Result<()> {
    // TODO handle visibility (don't generate bindings for private methods)
    // TODO handle constness
    let func = meth.func();
    debug_assert_eq!(func.param_tys.len(), func.param_names.len());

    let arg_names = arg_names(meth);
    let gen_fn_sig = |f: &mut W, self_arg| -> io::Result<()> {
        write!(f, "    fn {}({self}", meth.func().name, self = self_arg)?;
        write!(f, ", ")?;
        for (name, ty) in arg_names.iter().zip(meth.param_tys(db)) {
            write!(f, "{}: ", name)?;
            ty.gen(db, f)?;
            write!(f, ", ")?;
        }
        write!(f, ")")?;
        Ok(())
    };

    let trait_name = format!("{}_{}_Ext", st.name, func.name);

    // Create an extension trait for our method.
    writeln!(f, "trait {} {{", trait_name)?;
    gen_fn_sig(f, "self")?;
    writeln!(f, ";")?;
    writeln!(f, "}}")?;

    // impl the extension trait for NonNull<Struct>.
    writeln!(
        f,
        "impl {} for ::core::ptr::NonNull<{}> {{",
        trait_name, st.name
    )?;
    gen_fn_sig(f, "self")?;
    writeln!(f, " {{")?;
    writeln!(f, "        todo!()")?;
    writeln!(f, "    }}")?;
    writeln!(f, "}}")?;

    // Create a convenience wrapper for &mut self.
    writeln!(f, "impl {} {{", st.name)?;
    gen_fn_sig(f, "&mut self")?;
    writeln!(f, " {{")?;
    write!(f, "        ::core::ptr::NonNull::from(self).{}(", func.name)?;
    for arg in &arg_names {
        write!(f, "{}, ", arg)?;
    }
    writeln!(f, ")")?;
    writeln!(f, "    }}")?;
    writeln!(f, "}}")?;

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

impl Codegen for rs::Ty {
    fn gen(&self, db: &impl RsIr, f: &mut impl io::Write) -> io::Result<()> {
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
            Struct(id) => return write!(f, "{}", id.lookup(db).name),
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
            fn sum(self, c: i32, __1: i32, );
        }
        impl Foo_sum_Ext for ::core::ptr::NonNull<Foo> {
            fn sum(self, c: i32, __1: i32, ) {
                todo!()
            }
        }
        impl Foo {
            fn sum(&mut self, c: i32, __1: i32, ) {
                ::core::ptr::NonNull::from(self).sum(c, __1, )
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
