//! The cc_use proc macro.
//!
//! This represents one side of the cc_use logic. The other side is in the bindings generator.

extern crate proc_macro;

use cc_use_common::{CcUse, LibName};
use proc_macro2::TokenStream;
use quote::quote;
use std::path::PathBuf;
use syn::{parse_macro_input, punctuated::Punctuated, Ident, Token};

#[proc_macro]
pub fn cc_use(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mac = parse_macro_input!(input as CcUse);
    cc_use_impl(mac).into()
}

fn cc_use_impl(input: CcUse) -> TokenStream {
    let CcUse {
        header,
        lib_name,
        cc_paths,
        ..
    } = input;
    let native_lib = lib_name.clone().unwrap_or_else(|| LibName {
        name: PathBuf::from(header.path)
            .file_stem()
            .unwrap()
            .to_string_lossy()
            .to_string(),
        span: header.span,
    });
    let crate_name = Ident::new(&(native_lib.name + "_bind"), native_lib.span);
    let paths: Punctuated<_, Token![,]> = cc_paths.iter().collect();
    quote! {
        extern crate #crate_name;
        use #crate_name::{#paths};
    }
}
