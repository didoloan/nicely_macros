mod client;
mod request;

#[proc_macro_derive(Request, attributes(request))]
pub fn gen_request(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    request::gen_request_impl(input)
}

#[proc_macro_attribute]
pub fn gen_client(
    attrs: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    client::gen_client_impl(attrs, input)
}
