#[cfg(test)]
#[macro_use]
mod test_util;

#[macro_use]
mod util;

#[macro_use]
extern crate rental;

mod codegen;
mod diagnostics;
mod index;
mod ir;
mod libclang;
mod salsa_test;

use crate::diagnostics::DiagnosticsCtx;
use std::env;

pub struct Session {
    // TODO: opts
    diags: DiagnosticsCtx<String>,
}

impl Session {
    pub fn new() -> Self {
        Session {
            diags: DiagnosticsCtx::new(),
        }
    }

    #[cfg(test)]
    pub(crate) fn test() -> Self {
        Session {
            diags: DiagnosticsCtx::test(),
        }
    }
}

fn main() -> Result<(), libclang::Error> {
    let sess = Session::new();
    let filename = env::args().nth(1).expect("Usage: cargo run <cc_file>");
    let module = libclang::parse_and_lower(&sess, &filename.into())?;
    let rs_module = module.then(|m| m.to_rust(&sess));
    let errs = match rs_module.val() {
        Ok(rs_module) => codegen::perform_codegen(&sess, &rs_module).errs(),
        Err(errs) => errs,
    };
    errs.emit(&sess.diags);
    Ok(())
}
