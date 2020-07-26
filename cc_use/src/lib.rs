extern crate proc_macro;

use quote::quote;

#[proc_macro]
pub fn cc_use(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    quote!().into()
}
