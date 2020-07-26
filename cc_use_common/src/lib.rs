//! Types and parsing code shared between peasy and the cc_use macro.

use proc_macro2::Span;
use syn::{
    self,
    parse::{self, Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
    Token,
};

pub struct CcUse {
    pub header: Header,
    #[allow(unused)]
    pub lib_name: Option<LibName>,
    #[allow(unused)]
    pub cc_paths: Vec<CcPath>,
}

pub struct Header {
    pub path: String,
    pub is_system: bool,
    pub span: Span,
}

// TODO: This is to support automatic passing of flags from a sys crate's build script to us.
#[derive(Clone)]
pub struct LibName {
    pub name: String,
    pub span: Span,
}

pub type CcPath = syn::Path;

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
