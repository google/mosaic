use crate::{
    codegen,
    diagnostics::{
        self,
        db::{FileId, SourceFileCache},
        Diagnostic, Diagnostics, Outcome,
    },
    ir, libclang, Session, SourceFileKind,
};
use cc_use_common::{CcPath, CcUse};
use proc_macro2::Span;
use std::fs::File;
use std::{
    io::{self, Read},
    path::Path,
};
use syn::{self, spanned::Spanned};

const MACRO_NAME: &'static str = "cc_use";

pub(crate) fn process_rs_input(
    sess: &Session,
    index: &libclang::Index,
    input_path: &Path,
) -> io::Result<
    Outcome<(
        Vec<(libclang::ModuleContext, libclang::ParseErrors)>,
        Vec<codegen::Header>,
    )>,
> {
    let mut contents = String::new();
    File::open(input_path)?.read_to_string(&mut contents)?;

    let source = SourceFile {
        name: input_path.to_string_lossy().to_string(),
        contents: contents.clone(),
    };
    let file_id = sess.db.intern_source_file(SourceFileKind::Rs(source));

    Ok(parse_rs_file(sess, &contents, file_id).map(|macros| {
        let mut cc_modules = vec![];
        let mut headers = vec![];
        for cc_use in macros {
            let module_id = libclang::ModuleId::new(cc_modules.len() as _);
            let (ctx, err, header) =
                load_cc_module(sess, index, input_path, &cc_use, file_id, module_id);
            cc_modules.push((ctx, err));
            headers.push(header);
        }
        (cc_modules, headers)
    }))
}

fn parse_rs_file(sess: &Session, contents: &str, file_id: FileId) -> Outcome<Vec<CcUse>> {
    // TODO: we can improve performance by not parsing everything in the file.
    let ast = match syn::parse_file(&contents) {
        Ok(ast) => ast,
        Err(err) => return Outcome::from_parts(vec![], error(&sess.db, file_id, err)),
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
                        errs.append(error(&sess.db, file_id, err));
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
    sess: &Session,
    index: &libclang::Index,
    rs_src_path: &Path,
    cc_use: &CcUse,
    file_id: FileId,
    module_id: libclang::ModuleId,
) -> (
    libclang::ModuleContext,
    libclang::ParseErrors,
    codegen::Header,
) {
    let paths = cc_use
        .cc_paths
        .iter()
        .map(|path| (path.into(), span(&sess.db, file_id, path.span())))
        .collect();
    let (ctx, errs) = libclang::parse_with(sess, index, module_id, paths, |index| {
        let line = cc_use.header.span.start().line;
        let src = if cc_use.header.is_system {
            // TODO: This line directive doesn't have the intended effect. We can either figure
            // out a way to use the "presumed location" libclang gives us, or find a way to present
            // a better error ourselves.
            format!("#line {}\n#include <{}>", line, cc_use.header.path)
        } else {
            format!("#line {}\n#include \"{}\"", line, cc_use.header.path)
        };
        // Use the rust source as the path so it shows up in "not found" errors
        let mut parser = libclang::configure(index.parser(rs_src_path));
        let unsaved = clang::Unsaved::new(rs_src_path, src);
        // unwrap is okay because we know the "file" exists.
        parser.unsaved(&[unsaved]).parse().unwrap()
    });
    let header = match cc_use.header.is_system {
        true => codegen::Header::system(&cc_use.header.path),
        false => codegen::Header::local(&cc_use.header.path),
    };
    (ctx, errs, header)
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct SourceFile {
    name: String,
    contents: String,
}
impl SourceFile {
    pub(crate) fn get_name_and_contents(&self) -> (String, String) {
        (self.name.clone(), self.contents.clone())
    }
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

impl From<&CcPath> for ir::cc::Path {
    fn from(other: &CcPath) -> Self {
        other
            .segments
            .iter()
            .map(|segment| {
                assert!(segment.arguments.is_empty());
                ir::cc::Ident::from(segment.ident.to_string())
            })
            .collect()
    }
}
