use darling::FromMeta;
use syn::{self, DeriveInput};

pub(crate) fn gen_client_impl(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attr_args = syn::parse_macro_input!(attr as AttributeArg);

    if let AttributeArg::Ident(env_var) = &attr_args {
        if let Err(_) = dotenv::dotenv() {
            return syn::Error::new(
                env_var.span(),
                format!(
                    "Can't find .env file to load {}.\nPlace a .env file in project path.",
                    env_var.to_string()
                ),
            )
            .into_compile_error()
            .into();
        }
    }

    // Parse the annotated item (struct or enum)
    let DeriveInput { ident, data: _, .. } = syn::parse_macro_input!(input as DeriveInput);

    let mut builder_ident_str = ident.to_string();

    builder_ident_str.push_str("Builder");

    let builder_ident = syn::Ident::from_string(&builder_ident_str).unwrap();

    let base_url_func = match attr_args {
        AttributeArg::BaseUrl(url) => {
            let url = url.value();
            quote::quote! { std::borrow::Cow::Borrowed(#url) }
        },
        AttributeArg::Ident(ident) => {
            let key = ident.to_string();

            match std::env::var(key.trim()) {
                Ok(base_url) => {
                    if let Err(_) = url::Url::parse(&base_url) {
                        return ::syn::Error::new(
                            ident.span(),
                            "Invalid Url: Not a valid BaseUrl.",
                        )
                        .into_compile_error()
                        .into();
                    }
                }
                Err(_) => {
                    return syn::Error::new(ident.span(), format!("Environment variable {} should be present in a .env file for compile time validation.", key))
                        .into_compile_error()
                        .into();
                }
            }

            quote::quote! {
                let value = std::env::var(#key).unwrap();
                std::borrow::Cow::Owned(value)
            }
        }
    };

    quote::quote! {

        #[derive(Debug)]
        pub struct #ident<'a> {
            auth: ask_nicely::authentication::Authentication<'a>,
            client: reqwest::Client
        }

        #[derive(Debug)]
        pub struct #builder_ident<'a> {
            auth: ask_nicely::authentication::Authentication<'a>,
            builder: reqwest::ClientBuilder
        }

        impl<'a> #builder_ident<'a> {
            pub fn set_auth(mut self, auth: ask_nicely::authentication::Authentication<'a>) -> Self {
                self.auth = auth;
                self
            }

            pub fn set_timeout(mut self, timeout: std::time::Duration) -> Self {
                self.builder = self.builder.timeout(timeout);
                self
            }

            pub fn build(self) -> reqwest::Result<#ident<'a>> {
                let #builder_ident { auth, builder } = self;

                let client = builder.build()?;

                Ok(#ident {
                    auth: auth.clone(),
                    client
                })
            }

        }

        impl<'a> #ident<'a> {
            pub fn new(auth: ask_nicely::authentication::Authentication<'a>) -> #ident<'a> {
                #ident {
                    auth,
                    client: reqwest::Client::new()
                }
            }

            pub fn builder() -> #builder_ident<'a> {
                #builder_ident {
                    auth: ask_nicely::authentication::Authentication::None,
                    builder: reqwest::ClientBuilder::new()
                }
            }

            fn base_url(&'a self) -> std::borrow::Cow<'a, str> {
                #base_url_func
            }

            pub fn set_auth(&'a mut self, auth: ask_nicely::authentication::Authentication<'a>) -> &'a mut Self {
                self.auth = auth;
                self
            }

            pub fn set_client(&mut self, new_client: reqwest::Client) {
                self.client = new_client;
            }
        }

        impl<'a> ask_nicely::client::CanApiClient<'a> for #ident<'a> {
            fn get_auth(&'a self) -> &'a ask_nicely::authentication::Authentication<'a> {
                &self.auth
            }

            fn get_base_url(&self) -> String {
                self.base_url().into_owned()
            }

            fn get_client(&'a self) -> &'a reqwest::Client {
                &self.client
            }
        }

    }
    .into()
}

enum AttributeArg {
    BaseUrl(syn::LitStr),
    Ident(syn::Ident),
}

impl syn::parse::Parse for AttributeArg {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let peeked = input.lookahead1();
        if peeked.peek(syn::Ident) {
            Ok(AttributeArg::Ident(input.parse()?))
        } else if peeked.peek(syn::LitStr) {
            Ok(AttributeArg::BaseUrl(input.parse()?))
        } else {
            Err(syn::Error::new(input.span(), "Expected baseurl  or env variable name  \nSamples:\n\t \"http(s)://baseurl.com\" or BASE_URL"))
        }
    }
}

