//! Generates Rust code from IR.

use crate::diagnostics::{ok, Diagnostics, Outcome};
use crate::ir::rs;
use crate::Session;

pub fn perform_codegen(sess: &Session, mdl: &rs::Module) -> Outcome<()> {
    Diagnostics::build(|diags| {
        for (_, st) in mdl.structs() {
            let errs = codegen_struct(sess, &st).errs();
            diags.append(errs);
        }
    })
    .into()
}

fn codegen_struct(_sess: &Session, st: &rs::Struct) -> Outcome<()> {
    println!("#[repr(C, align({}))]", st.align);
    println!("struct {} {{", st.name);
    for field in &st.fields {
        println!("  {}: {},", field.name, field.ty);
    }
    println!("}}");
    ok(())
}
