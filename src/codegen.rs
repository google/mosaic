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

impl Codegen for rs::Struct {
    fn gen(&self, db: &impl RsIr, f: &mut impl io::Write) -> io::Result<()> {
        writeln!(f, "#[repr(C, align({}))]", self.align)?;
        writeln!(f, "struct {} {{", self.name)?;
        for field in &self.fields {
            write!(f, "    {}: ", field.name)?;
            field.ty.gen(db, f)?;
            writeln!(f, ",")?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
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
            struct Pod {
                a: i32,
                b: i32,
                e: i8,
                c: f64,
                d: f64,
            }
        "#);
    }

    #[test]
    fn nested_struct() {
        let mut sess = Session::new();
        cpp_to_rs!(sess, {
            // TODO codegen a struct for Foo
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
            struct Foo {
                a: i32,
                b: i32,
            }
            #[repr(C, align(4))]
            struct Bar {
                c: i8,
                d: i8,
                foo: Foo,
            }
        "#);
    }
}
