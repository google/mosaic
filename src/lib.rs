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

use cc_use::RsSource;
use diagnostics::DiagnosticsCtx;

use salsa;
use std::{
    ffi::OsString,
    io::{self, Write},
    path::{Path, PathBuf},
};
use structopt::StructOpt;

#[salsa::database(
    cc_use::RsSourceStorage,
    libclang::AstContextStorage,
    libclang::AstMethodsStorage,
    diagnostics::db::SourceFileCacheStorage,
    ir::IrMethodsStorage,
    ir::cc::CcSourceBindingsStorage,
    ir::cc::RsIrStorage,
    ir::rs::RsTargetStorage
)]
pub(crate) struct Database {
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

    /// the name of the binding crate to generate
    #[structopt(long)]
    crate_name: Option<OsString>,

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
    let out_basename = opts.crate_name.unwrap_or_else(|| {
        let mut name = input_path.file_stem().unwrap().to_os_string();
        name.push("_bind");
        name
    });
    let out_base = out_dir.join(out_basename);

    // If the input is a Rust file, parse it for cc_use! macros and then parse the header files it
    // points to. Otherwise, parse the input file directly as C++.
    let mut sess = Session::new();
    let index = libclang::create_index();
    let (rs_headers, cli_headers);
    let (cc_modules, headers) = if let Some("rs") = input_path.extension().and_then(|p| p.to_str())
    {
        sess.db
            .set_rs_source_root(Some(cc_use::SourceFile::intern_from_path(
                &sess.db,
                &input_path,
            )?));
        let (module_ids, errs) = sess.db.module_ids().split();
        errs.emit(&sess.db, &sess.diags);
        let modules = module_ids
            .iter()
            .map(|module_id| cc_use::cc_module_from_rs(&sess.db, &index, *module_id))
            .collect();
        rs_headers = sess.db.headers().skip_errs();
        (modules, &*rs_headers)
    } else {
        sess.db.set_rs_source_root(None);
        let include_path = input_path
            .file_name()
            .unwrap()
            .to_str()
            .ok_or("Input filename must be valid UTF-8")?;
        let module_id = libclang::ModuleId::new(0);
        cli_headers = vec![ir::bindings::Header {
            path: include_path.to_string(),
            is_system: false,
            span: None,
        }];
        (
            vec![libclang::parse(&sess.db, &index, module_id, &input_path)],
            &*cli_headers,
        )
    };

    let out_rs = tempfile::Builder::new().tempfile_in(out_dir)?;
    let out_cc = tempfile::Builder::new().tempfile_in(out_dir)?;
    let success = run_generator(&mut sess, cc_modules, headers, &out_rs, &out_cc);
    if !success {
        return Ok(101);
    }
    out_rs.persist(out_base.with_extension("rs"))?;
    out_cc.persist(out_base.with_extension("cc"))?;

    Ok(0)
}

fn run_generator(
    sess: &mut Session,
    parsed_cc_modules: Vec<(libclang::ModuleContext, libclang::ParseErrors)>,
    headers: &[ir::bindings::Header],
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

    let (cc_modules, parse_errs): (Vec<_>, Vec<_>) = parsed_cc_modules.into_iter().unzip();

    let diags = &sess.diags;
    libclang::set_ast(&mut sess.db, cc_modules, |db| {
        for errs in parse_errs {
            errs.to_diagnostics(db).emit(db, diags);
        }

        use ir::rs::RsTarget;
        let rs_module = db.rs_bindings();
        let (rs_module, errs) = rs_module.to_ref().split();
        errs.clone().emit(db, diags);

        if diags.has_errors() {
            return false;
        }
        codegen::perform_codegen(db, &rs_module, headers, false, outputs).expect("Codegen failed");
        true
    })
}
