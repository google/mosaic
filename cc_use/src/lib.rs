//! The cc_use proc macro.
//!
//! This represents one side of the cc_use logic. The other side is in the bindings generator.

extern crate proc_macro;

use cc_use_common::CcUse;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use std::env;
use syn::{parse_macro_input, punctuated::Punctuated, Ident, Token};

#[proc_macro]
pub fn cc_use(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mac = parse_macro_input!(input as CcUse);
    cc_use_impl(mac).into()
}

fn cc_use_impl(input: CcUse) -> TokenStream {
    // FIXME: We should allow setting the name of the binding crate explicitly since external build
    // systems will have to care about that.
    let crate_name = env::var("PKG_NAME")
        .or(env::var("CARGO_PKG_NAME"))
        .expect("CARGO_PKG_NAME or PKG_NAME must be defined")
        .to_owned();
    let bind_crate_name = Ident::new(&(crate_name + "_bind"), Span::call_site());
    let paths: Punctuated<_, Token![,]> = input.cc_paths.iter().collect();
    quote! {
        extern crate #bind_crate_name;
        use #bind_crate_name::{#paths};
    }
}
