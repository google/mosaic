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

use crate::diagnostics::DiagnosticsCtx;
use salsa;
use std::{env, io};

pub(crate) use libclang::File;

#[salsa::database(
    libclang::AstMethodsStorage,
    diagnostics::db::BasicFileCacheStorage,
    ir::IrMethodsStorage,
    ir::cc::RsIrStorage
)]
pub struct Database {
    runtime: salsa::Runtime<Database>,
}

pub trait FileInterner: libclang::AstMethods {}
impl FileInterner for Database {}

impl salsa::Database for Database {
    fn salsa_runtime(&self) -> &salsa::Runtime<Database> {
        &self.runtime
    }
    fn salsa_runtime_mut(&mut self) -> &mut salsa::Runtime<Database> {
        &mut self.runtime
    }
}

impl Database {
    pub fn new() -> Database {
        Database {
            runtime: salsa::Runtime::default(),
        }
    }
}

pub struct Session {
    // TODO: opts
    diags: DiagnosticsCtx,
    db: Database,
}

impl Session {
    pub fn new() -> Self {
        Session {
            diags: DiagnosticsCtx::new(),
            db: Database::new(),
        }
    }

    #[cfg(test)]
    pub(crate) fn test() -> Self {
        Session {
            diags: DiagnosticsCtx::test(),
            db: Database::new(),
        }
    }
}

fn main() -> Result<(), libclang::Error> {
    use ir::cc::RsIr;

    let mut sess = Session::new();
    let filename = env::args().nth(1).expect("Usage: cargo run <cc_file>");

    let parse = libclang::parse(&sess, &filename.into());
    let diags = &sess.diags;
    libclang::set_ast(&mut sess.db, parse, |db| {
        let rs_module = db.rs_bindings();
        match rs_module.to_ref().val() {
            Ok(rs_module) => {
                let out = io::stdout();
                let mut out = out.lock();
                codegen::perform_codegen(db, &rs_module, &mut out).expect("Codegen failed");
            }
            Err(errs) => errs.clone().emit(db, diags),
        };
    });

    Ok(())
}
