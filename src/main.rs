#[cfg(test)]
#[macro_use]
mod test_util;

#[macro_use]
mod util;

mod codegen;
mod diagnostics;
mod index;
mod ir;
mod libclang;

use crate::diagnostics::DiagnosticsCtx;
use std::env;

pub struct Session {
    // TODO: opts
    diags: DiagnosticsCtx<String>,
}

impl Session {
    fn new() -> Self {
        Session {
            diags: DiagnosticsCtx::new(),
        }
    }
}

fn main() -> Result<(), libclang::Error> {
    let sess = Session::new();
    let filename = env::args().nth(1).expect("Usage: cargo run <cc_file>");
    let module = libclang::parse_and_lower(&sess, &filename.into())?;
    module.check();
    codegen::perform_codegen(&sess, &module);
    // TODO: check sess.diags.has_errors()
    Ok(())
}
