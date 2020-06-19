#![feature(proc_macro_hygiene)]

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
use std::{env, io, path::PathBuf};

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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    use ir::cc::RsIr;

    let mut sess = Session::new();
    let input_path: PathBuf = env::args()
        .nth(1)
        .expect("Usage: cargo run <cc_file>")
        .into();
    if !input_path.is_file() {
        eprintln!(
            "Error: input must exist and be a file: {}",
            input_path.to_string_lossy()
        );
        std::process::exit(1);
    }
    let out_dir = input_path.parent().unwrap();
    let out_base = {
        let mut name = input_path.file_stem().unwrap().to_os_string();
        name.push("_bind");
        input_path.with_file_name(name)
    };
    let include_path = input_path
        .file_name()
        .unwrap()
        .to_str()
        .ok_or("Input filename must be valid UTF-8")?
        .to_owned();

    let out_rs = tempfile::Builder::new().tempfile_in(out_dir)?;
    let out_cc = tempfile::Builder::new().tempfile_in(out_dir)?;

    let parse = libclang::parse(&sess, &input_path.into());
    let diags = &sess.diags;
    libclang::set_ast(&mut sess.db, parse, |db| {
        let rs_module = db.rs_bindings();
        match rs_module.to_ref().val() {
            Ok(rs_module) => {
                let mut rs_writer = io::BufWriter::new(&out_rs);
                let mut cc_writer = io::BufWriter::new(&out_cc);
                let outputs = codegen::Outputs {
                    rs: Some(codegen::CodeWriter::new(&mut rs_writer)),
                    cc: Some(codegen::CodeWriter::new(&mut cc_writer)),
                    hdr: None,
                };
                codegen::perform_codegen(db, &rs_module, &include_path, false, outputs)
                    .expect("Codegen failed");
            }
            Err(errs) => errs.clone().emit(db, diags),
        };
    });

    out_rs.persist(out_base.with_extension("rs"))?;
    out_cc.persist(out_base.with_extension("cc"))?;

    Ok(())
}
