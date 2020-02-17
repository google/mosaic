//! Generates Rust code from IR.

use crate::ir::{cc, rs};
use crate::Session;

pub fn perform_codegen(sess: &Session, mdl: &cc::Module) {
    for (_, st) in mdl.structs() {
        let rust_st = st.to_rust(sess);
        codegen_struct(sess, &rust_st);
    }
}

fn codegen_struct(_sess: &Session, st: &rs::Struct) {
    println!("#[repr(C, align({}))]", st.align);
    println!("struct {} {{", st.name);
    for field in &st.fields {
        println!("  {}: {},", field.name, field.ty);
    }
    println!("}}")
}
