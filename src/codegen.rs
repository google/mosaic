//! Generates Rust code from IR.

use crate::ir::*;

pub fn perform_codegen(mdl: &Module) {
    for st in &mdl.structs {
        codegen_struct(st);
    }
}

fn codegen_struct(st: &Struct) {
    // TODO: Check that field offsets and final size line up.
    // TODO: align
    println!("#[repr(C)]");
    println!("struct {} {{", st.name);
    for field in &st.fields {
        print!("  {}: ", field.name);
        match &field.ty {
            Ty::Int => print!("i32"),
            Ty::UInt => print!("u32"),
            Ty::CharS | Ty::SChar => print!("i8"),
            Ty::CharU | Ty::UChar => print!("u8"),
            Ty::Float => print!("f32"),
            Ty::Double => print!("f64"),
            other => panic!("{}::{}: Unsupported type {:?}", st.name, field.name, other),
        }
        println!(",");
    }
    println!("}}")
}
