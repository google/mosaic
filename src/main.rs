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
use salsa;
use std::env;

pub(crate) use libclang::File;

#[salsa::database(
    libclang::db::AstMethodsStorage,
    diagnostics::db::BasicFileCacheStorage,
    ir::cc::RsIrStorage
)]
pub struct Database {
    runtime: salsa::Runtime<Database>,
}

pub trait FileInterner: libclang::db::AstMethods {}
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
    let rs_module = libclang::set_ast(&mut sess.db, parse, |db| db.rs_ir());

    match rs_module.to_ref().val() {
        Ok(rs_module) => {
            let errs = codegen::perform_codegen(&sess, &rs_module).errs();
            errs.emit(&sess.db, &sess.diags);
        }
        Err(errs) => errs.clone().emit(&sess.db, &sess.diags),
    };

    Ok(())
}
