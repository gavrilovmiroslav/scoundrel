use proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn;
use syn::__private::{Span, TokenStream2};

#[proc_macro_derive(BindableInputAction)]
pub fn bindable_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_bindable(&ast)
}

fn impl_bindable(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let mut pats = TokenStream2::new();

    match &ast.data {
        syn::Data::Enum(enum_item) => {
            for id in enum_item.variants.clone().into_iter().map(|v| v.ident) {
                let quoted = syn::LitStr::new(format!("{}", id).as_str(), Span::call_site());
                let pat = quote! { Some(#name::#id) };

                pats.extend(quote! { #quoted => { #pat }, });
            }

            let gen = quote! {
                impl BindableInputAction for #name {
                    fn from_str<S: AsRef<str>>(s: S) -> Option<Self> {
                        match s.as_ref() {
                            #pats
                            _ => None
                        }
                    }
                }
            };

            gen.into()
        }

        _ => panic!("Bindable input actions only work with enums"),
    }
}
