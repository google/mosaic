use crate::{
    diagnostics::{
        self,
        db::{FileId, SourceFileInterner},
        Diagnostics, Outcome,
    },
    ir, libclang, Session, SourceFileKind,
};
use diagnostics::{db::BasicFileCache, Diagnostic};
use proc_macro2::Span;
use std::fs::File;
use std::{
    io::{self, Read},
    path::Path,
};
use syn::{
    self,
    parse::{self, Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
    Token,
};

const MACRO_NAME: &'static str = "cc_use";

struct CcUse {
    header: Header,
    #[allow(unused)]
    lib_name: Option<LibName>,
    #[allow(unused)]
    cc_paths: Vec<CcPath>,
}

struct Header {
    path: String,
    is_system: bool,
    span: Span,
}

// TODO: This is to support automatic passing of flags from a sys crate's build script to us.
#[allow(unused)]
struct LibName {
    name: String,
    span: Span,
}

type CcPath = syn::Path;

impl Parse for Header {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::LitStr) {
            let hdr: syn::LitStr = input.parse()?;
            let mut path = hdr.value();
            let is_system = path.starts_with('<') && path.ends_with('>');
            if is_system {
                path = path[1..path.len() - 1].to_owned();
            }
            Ok(Header {
                path,
                span: hdr.span(),
                is_system,
            })
        } else if lookahead.peek(syn::token::Lt) {
            let lt = input.parse::<syn::token::Lt>()?;
            let parse_hdr = || -> parse::Result<_> {
                // For now we only support single-word names, like those in the standard library,
                // in bare angle brackets. TODO: Ideally we'd support any valid filename.
                let hdr: syn::Ident = input.parse()?;
                input.parse::<syn::token::Gt>()?;
                Ok(hdr)
            };
            let hdr = match parse_hdr() {
                Ok(hdr) => hdr,
                Err(mut e) => {
                    e.combine(syn::Error::new(
                        lt.span(),
                        "Try putting system paths inside quotes: \"<foo/bar.h>\"",
                    ));
                    return Err(e);
                }
            };
            Ok(Header {
                path: hdr.to_string(),
                span: hdr.span(),
                is_system: true,
            })
        } else {
            Err(lookahead.error())
        }
    }
}

// in "foo"
impl Parse for LibName {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        input.parse::<Token![in]>()?;
        let name: syn::LitStr = input.parse()?;
        Ok(LibName {
            name: name.value(),
            span: name.span(),
        })
    }
}

// cc_use!("foo.h" [in "foo"], item1[, item2[, ...]])
impl Parse for CcUse {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let header = input.parse()?;

        let lookahead = input.lookahead1();
        let lib_name = if lookahead.peek(Token![in]) {
            Some(input.parse()?)
        } else if lookahead.peek(Token![,]) {
            None
        } else {
            return Err(lookahead.error());
        };

        input.parse::<Token![,]>()?;

        let paths: Punctuated<CcPath, Token![,]> = input.parse_terminated(CcPath::parse)?;
        let cc_paths = paths.iter().cloned().collect();

        Ok(CcUse {
            header,
            lib_name,
            cc_paths,
        })
    }
}

pub(crate) fn process_rs_input(
    sess: &Session,
    index: &libclang::Index,
    input_path: &Path,
) -> io::Result<Outcome<Vec<libclang::ModuleContext>>> {
    let mut content = String::new();
    File::open(input_path)?.read_to_string(&mut content)?;

    let source = SourceFile {
        name: input_path.to_string_lossy().to_string(),
        contents: content.clone(),
    };
    let file_id = sess.db.intern_source_file(SourceFileKind::Rs(source));

    // TODO: we can improve performance by not parsing everything in the file.
    let ast = match syn::parse_file(&content) {
        Ok(ast) => ast,
        Err(err) => return Ok(Outcome::from_parts(vec![], error(&sess.db, file_id, err))),
    };

    // TODO: actually resolve names enough to know if this is our macro or not, and recognize
    // alternate names.
    let mut macros = vec![];
    for item in ast.items {
        match item {
            syn::Item::Macro(m)
                if MACRO_NAME == m.mac.path.segments.last().unwrap().ident.to_string() =>
            {
                macros.push(m);
            }
            _ => (),
        }
    }

    let mut cc_modules = vec![];
    let mut errs = Diagnostics::new();
    for mac in macros {
        let cc_use: CcUse = match mac.mac.parse_body() {
            Ok(cc_use) => cc_use,
            Err(err) => {
                errs.append(error(&sess.db, file_id, err));
                continue;
            }
        };
        match load_cc_module(sess, index, input_path, &cc_use, file_id) {
            Ok(module) => cc_modules.push(module),
            Err(err) => errs.append(err.into()),
        }
    }

    Ok(Outcome::from_parts(cc_modules, errs))
}

fn load_cc_module(
    sess: &Session,
    index: &libclang::Index,
    rs_src_path: &Path,
    cc_use: &CcUse,
    file_id: FileId,
) -> Result<libclang::ModuleContext, libclang::SourceError> {
    let paths = cc_use
        .cc_paths
        .iter()
        .map(|path| (path.into(), span(&sess.db, file_id, path.span())))
        .collect();
    Ok(libclang::parse_with(sess, index, paths, |index| {
        let line = cc_use.header.span.start().line;
        let src = if cc_use.header.is_system {
            format!("#line {}\n#include <{}>", line, cc_use.header.path)
        } else {
            format!("#line {}\n#include \"{}\"", line, cc_use.header.path)
        };
        // Use the rust source as the path so it shows up in "not found" errors
        let mut parser = libclang::configure(index.parser(rs_src_path));
        let unsaved = clang::Unsaved::new(rs_src_path, src);
        // TODO don't unwrap
        parser.unsaved(&[unsaved]).parse().unwrap()
    }))
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

fn span(db: &impl BasicFileCache, file_id: FileId, span: Span) -> diagnostics::Span {
    // In proc_macro2 the lines are 1-indexed and columns are 0-indexed, whereas in codespan they
    // are both 0-indexed.
    let start =
        diagnostics::Location::new(span.start().line as u32 - 1, span.start().column as u32);
    let end = diagnostics::Location::new(span.end().line as u32 - 1, span.end().column as u32);
    diagnostics::Span::from_location(db, file_id, start, end)
}

fn error(db: &impl BasicFileCache, file_id: FileId, input: syn::Error) -> Diagnostics {
    Diagnostics::build(|errs| {
        for err in input {
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
