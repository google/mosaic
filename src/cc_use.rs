use crate::{
    diagnostics::{
        self,
        db::{FileId, SourceFileCache},
        Diagnostic, Diagnostics, Outcome,
    },
    ir, libclang,
};
use cc_use_common::{CcPath, CcUse};
use proc_macro2::Span;
use std::fs::File;
use std::{
    collections::BTreeMap,
    io::{self, Read},
    path::Path,
    sync::Arc,
};
use syn::{self, spanned::Spanned};

const MACRO_NAME: &'static str = "cc_use";

/// Provides access to Rust source files.
#[salsa::query_group(RsSourceStorage)]
pub trait RsSource: SourceFileCache {
    /// The source root of the Rust crate we're generating bindings for, if any.
    #[salsa::input]
    fn rs_source_root(&self) -> Option<FileId>;
}

/// Generates IR for `cc_use!` imports in Rust source.
#[salsa::query_group(RsImportIrStorage)]
pub trait RsImportIr: RsSource {
    #[doc(hidden)]
    fn headers_with_imports(&self) -> Arc<Outcome<Vec<HeaderInfo>>>;

    /// The range of `ModuleId`s used.
    ///
    /// WARNING: This is only useful when using Rust source. `AstMethods` has the method that
    /// always works.
    fn module_ids(&self) -> Outcome<Arc<[ir::bindings::ModuleId]>>;

    /// The set of headers imported in the Rust crate.
    fn headers(&self) -> Outcome<Arc<[ir::bindings::Header]>>;

    /// The set of (top-level) imports in the Rust crate.
    fn imports(&self) -> Outcome<Arc<[ir::bindings::Import]>>;

    /// The set of (top-level) imports in the Rust crate for a given `ModuleId`.
    fn imports_for(&self, mdl: ir::bindings::ModuleId) -> Outcome<Arc<[ir::bindings::Import]>>;
}

/// Parses the C++ header file for the given `ModuleId`.
// This should probably be a query, but the libclang types don't easily go in salsa.
pub(crate) fn cc_module_from_rs(
    db: &impl RsImportIr,
    index: &libclang::Index,
    module_id: ir::bindings::ModuleId,
) -> (libclang::ModuleContext, libclang::ParseErrors) {
    let rs_source_path = db.rs_source_root().unwrap().name(db);
    let hdrs = db.headers_with_imports();
    // skip_errs okay since the user must go through an outcome with these errors to get module_id.
    let hdr = &hdrs.to_ref().skip_errs()[module_id.as_usize()];
    let parse = load_cc_module(
        db,
        index,
        Path::new(&rs_source_path),
        &hdr.header,
        hdr.module_id,
    );
    parse
}

#[doc(hidden)]
#[derive(Debug, Eq, PartialEq)]
pub struct HeaderInfo {
    module_id: ir::bindings::ModuleId,
    header: ir::bindings::Header,
    imports: Vec<ir::bindings::Import>,
}

// Parse Rust file and return IR imports grouped by header.
fn headers_with_imports(db: &impl RsImportIr) -> Arc<Outcome<Vec<HeaderInfo>>> {
    let file_id = db.rs_source_root().unwrap();
    let headers = parse_rs_file(db, &file_id.contents(db), file_id).map(|macros| {
        let mut headers = BTreeMap::new();
        for mac in &macros {
            headers
                .entry((&mac.header.path, mac.header.is_system))
                .or_insert((mac.header.span, Vec::new()))
                .1
                .push(&mac.cc_paths);
        }
        headers
            .iter()
            .enumerate()
            .map(|(id, (hdr, macros))| {
                let module_id = ir::bindings::ModuleId::new(id as _);
                let header = ir::bindings::Header {
                    path: hdr.0.clone(),
                    is_system: hdr.1,
                    span: Some(span(db, file_id, macros.0)),
                };
                let imports = macros
                    .1
                    .iter()
                    .flat_map(|cc_paths| {
                        cc_paths.iter().map(|path| ir::bindings::Import {
                            mdl: module_id,
                            path: path.into(),
                            span: span(db, file_id, path.span()),
                        })
                    })
                    .collect();
                HeaderInfo {
                    module_id,
                    header,
                    imports,
                }
            })
            .collect()
    });
    Arc::new(headers)
}

fn module_ids(db: &impl RsImportIr) -> Outcome<Arc<[ir::bindings::ModuleId]>> {
    db.headers_with_imports().to_ref().map(|hdrs| {
        (0..(hdrs.len() as _))
            .map(ir::bindings::ModuleId::new)
            .collect()
    })
}

fn headers(db: &impl RsImportIr) -> Outcome<Arc<[ir::bindings::Header]>> {
    db.headers_with_imports()
        .to_ref()
        .map(|data| data.iter().map(|hdr| hdr.header.clone()).collect())
}

fn imports(db: &impl RsImportIr) -> Outcome<Arc<[ir::bindings::Import]>> {
    if db.rs_source_root().is_none() {
        return Outcome::from_ok(Arc::new([]));
    }
    db.headers_with_imports().to_ref().map(|data| {
        data.iter()
            .flat_map(|hdr| hdr.imports.iter().cloned())
            .collect()
    })
}

fn imports_for(
    db: &impl RsImportIr,
    mdl: ir::bindings::ModuleId,
) -> Outcome<Arc<[ir::bindings::Import]>> {
    if db.rs_source_root().is_none() {
        return Outcome::from_ok(Arc::new([]));
    }
    db.headers_with_imports().to_ref().map(|data| {
        let mut headers = data.iter().filter(|hdr| hdr.module_id == mdl);
        let header = headers.next().unwrap();
        debug_assert!(headers.next().is_none());
        header.imports.clone().into()
    })
}

fn parse_rs_file(
    db: &impl SourceFileCache,
    contents: &str,
    file_id: FileId,
) -> Outcome<Vec<CcUse>> {
    // TODO: we can improve performance by not parsing everything in the file.
    let ast = match syn::parse_file(&contents) {
        Ok(ast) => ast,
        Err(err) => return Outcome::from_parts(vec![], error(db, file_id, err)),
    };

    // TODO: actually resolve names enough to know if this is our macro or not, and recognize
    // alternate names.
    let mut macros = vec![];
    let mut errs = Diagnostics::new();
    for item in ast.items {
        match item {
            syn::Item::Macro(m)
                if MACRO_NAME == m.mac.path.segments.last().unwrap().ident.to_string() =>
            {
                let cc_use: CcUse = match m.mac.parse_body() {
                    Ok(cc_use) => cc_use,
                    Err(err) => {
                        errs.append(error(db, file_id, err));
                        continue;
                    }
                };
                macros.push(cc_use);
            }
            _ => (),
        }
    }
    Outcome::from_parts(macros, errs)
}

fn load_cc_module(
    db: &impl SourceFileCache,
    index: &libclang::Index,
    rs_src_path: &Path,
    header: &ir::bindings::Header,
    module_id: ir::bindings::ModuleId,
) -> (libclang::ModuleContext, libclang::ParseErrors) {
    let (ctx, errs) = libclang::parse_with(db, index, module_id, |index| {
        let line = header.span.as_ref().map(|_| 1).unwrap(); // TODO
        let src = if header.is_system {
            // TODO: This line directive doesn't have the intended effect. We can either figure
            // out a way to use the "presumed location" libclang gives us, or find a way to present
            // a better error ourselves.
            format!("#line {}\n#include <{}>", line, header.path)
        } else {
            format!("#line {}\n#include \"{}\"", line, header.path)
        };
        // Use the rust source as the path so it shows up in "not found" errors
        let mut parser = libclang::configure(index.parser(rs_src_path));
        let unsaved = clang::Unsaved::new(rs_src_path, src);
        // unwrap is okay because we know the "file" exists.
        parser.unsaved(&[unsaved]).parse().unwrap()
    });
    (ctx, errs)
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct SourceFile(Arc<SourceFileInner>);
impl SourceFile {
    pub(crate) fn intern_from_path(
        db: &impl SourceFileCache,
        input_path: &Path,
    ) -> io::Result<FileId> {
        Ok(db.intern_source_file(crate::SourceFileKind::Rs(Self::from_path(input_path)?)))
    }

    fn from_path(path: &Path) -> io::Result<Self> {
        let mut contents = String::new();
        File::open(path)?.read_to_string(&mut contents)?;
        Ok(SourceFile(Arc::new(SourceFileInner {
            name: path.to_string_lossy().to_string(),
            contents,
        })))
    }

    pub(crate) fn get_name_and_contents(&self) -> (String, String) {
        (self.0.name.clone(), self.0.contents.clone())
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct SourceFileInner {
    name: String,
    contents: String,
}

fn span(db: &impl SourceFileCache, file_id: FileId, span: Span) -> diagnostics::Span {
    // In proc_macro2 the lines are 1-indexed and columns are 0-indexed, whereas in codespan they
    // are both 0-indexed.
    let start =
        diagnostics::Location::new(span.start().line as u32 - 1, span.start().column as u32);
    let end = diagnostics::Location::new(span.end().line as u32 - 1, span.end().column as u32);
    diagnostics::Span::from_location(db, file_id, start, end)
}

fn error(db: &impl SourceFileCache, file_id: FileId, input: syn::Error) -> Diagnostics {
    Diagnostics::build(|errs| {
        for err in input {
            // For now we duplicate the error message as both the header and label text.
            errs.add(Diagnostic::error(
                err.to_string(),
                span(db, file_id, err.span()).label(err.to_string()),
            ));
        }
    })
}

impl From<&CcPath> for ir::bindings::Path {
    fn from(other: &CcPath) -> Self {
        other
            .segments
            .iter()
            .map(|segment| {
                assert!(segment.arguments.is_empty());
                ir::bindings::PathComponent::from(ir::bindings::Ident::from(
                    segment.ident.to_string(),
                ))
            })
            .collect()
    }
}
