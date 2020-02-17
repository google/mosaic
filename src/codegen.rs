//! Generates Rust code from IR.

use crate::ir::rs;
use crate::Session;

pub fn perform_codegen(sess: &Session, mdl: &rs::Module) {
    for (_, st) in mdl.structs() {
        codegen_struct(sess, &st);
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
