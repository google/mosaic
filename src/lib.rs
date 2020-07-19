#![feature(proc_macro_hygiene)]

#[cfg(test)]
#[macro_use]
mod test_util;

#[macro_use]
mod util;

#[macro_use]
extern crate rental;

mod cc_use;
mod codegen;
mod diagnostics;
mod ir;
mod libclang;

use crate::diagnostics::DiagnosticsCtx;

use salsa;
use std::{
    io,
    path::{Path, PathBuf},
};
use structopt::StructOpt;

use io::Write;

#[salsa::database(
    libclang::AstContextStorage,
    libclang::AstMethodsStorage,
    diagnostics::db::SourceFileCacheStorage,
    ir::IrMethodsStorage,
    ir::cc::RsIrStorage
)]
pub struct Database {
    runtime: salsa::Runtime<Database>,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum SourceFileKind {
    Cc(libclang::SourceFile),
    Rs(cc_use::SourceFile),
}
impl SourceFileKind {
    fn get_name_and_contents(&self, db: &impl SourceFileLookup) -> (String, String) {
        match self {
            SourceFileKind::Cc(src) => src.get_name_and_contents(db),
            SourceFileKind::Rs(src) => src.get_name_and_contents(),
        }
    }
}
pub trait SourceFileLookup: libclang::AstContext {}
impl SourceFileLookup for Database {}

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

/// Generate bindings from C++ for Rust.
#[derive(StructOpt)]
struct Opts {
    /// where to put output files
    #[structopt(long)]
    out_dir: Option<String>,

    /// path to the C++ header file to generate bindings for
    input: String,
}

pub fn main() -> Result<i32, Box<dyn std::error::Error>> {
    let opts = Opts::from_args();
    let input_path = PathBuf::from(&opts.input);
    if !input_path.is_file() {
        eprintln!(
            "Error: input must exist and be a file: {}",
            input_path.to_string_lossy()
        );
        return Ok(1);
    }
    let out_dir = opts
        .out_dir
        .as_ref()
        .map(Path::new)
        .unwrap_or_else(|| input_path.parent().unwrap());
    let out_base = {
        let mut name = input_path.file_stem().unwrap().to_os_string();
        name.push("_bind");
        out_dir.join(name)
    };
    let include_path = input_path
        .file_name()
        .unwrap()
        .to_str()
        .ok_or("Input filename must be valid UTF-8")?
        .to_owned();

    // If the input is a Rust file, parse it for cc_use! macros and then parse the header files it
    // points to. Otherwise, parse the input file directly as C++.
    let mut sess = Session::new();
    let index = libclang::create_index();
    let cc_modules = if let Some("rs") = input_path.extension().and_then(|p| p.to_str()) {
        let (modules, errs) = cc_use::process_rs_input(&sess, &index, &input_path)?.split();
        errs.emit(&sess.db, &sess.diags);
        modules
    } else {
        vec![libclang::parse(&sess, &index, &input_path)]
    };

    let out_rs = tempfile::Builder::new().tempfile_in(out_dir)?;
    let out_cc = tempfile::Builder::new().tempfile_in(out_dir)?;
    let success = run_generator(&mut sess, cc_modules, &include_path, &out_rs, &out_cc);
    if !success {
        return Ok(101);
    }
    out_rs.persist(out_base.with_extension("rs"))?;
    out_cc.persist(out_base.with_extension("cc"))?;

    Ok(0)
}

fn run_generator(
    sess: &mut Session,
    cc_modules: Vec<libclang::ModuleContext>,
    include_path: &str,
    out_rs: impl Write,
    out_cc: impl Write,
) -> bool {
    let mut rs_writer = io::BufWriter::new(out_rs);
    let mut cc_writer = io::BufWriter::new(out_cc);
    let outputs = codegen::Outputs {
        rs: Some(codegen::CodeWriter::new(&mut rs_writer)),
        cc: Some(codegen::CodeWriter::new(&mut cc_writer)),
        hdr: None,
    };

    let diags = &sess.diags;
    libclang::set_ast(&mut sess.db, cc_modules, |db| {
        use ir::cc::RsIr;
        let rs_module = db.rs_bindings();
        let (rs_module, errs) = rs_module.to_ref().split();
        errs.clone().emit(db, diags);
        if diags.has_errors() {
            return false;
        }
        codegen::perform_codegen(db, &rs_module, &include_path, false, outputs)
            .expect("Codegen failed");
        true
    })
}
