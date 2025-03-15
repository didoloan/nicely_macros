use darling::{FromDeriveInput, FromField, FromMeta};
use quote::ToTokens;
use syn::spanned::Spanned;

#[derive(Debug, darling::FromDeriveInput)]
#[darling(attributes(request))]
pub(crate) struct RequestAttr {
    #[darling(default)]
    pub authed: bool,
    pub response: syn::Ident,
    pub method: syn::LitStr,
    pub path: syn::LitStr,
    pub get_data: syn::Ident,
}

#[derive(Debug, darling::FromField)]
#[darling(attributes(request))]
struct RequestFieldBodyAttr {
    #[darling(default)]
    pub body: bool,
}

fn get_body_tokens_with_payload_type(
    fields: &syn::Fields,
    data_type: &syn::Ident,
    method: &syn::LitStr,
    body_type_full: proc_macro2::TokenStream,
) -> Result<(proc_macro2::TokenStream, proc_macro2::TokenStream), syn::Error> {
    let mut body_fields = fields.iter().filter_map(|f| {
        let field_req_attr_opt = RequestFieldBodyAttr::from_field(f).ok();

        match field_req_attr_opt {
            Some(body_attr) => {
                if body_attr.body {
                    Some(f)
                } else {
                    None
                }
            }
            None => None,
        }
    });

    let field_with_body_opt = match body_fields.clone().count() {
        0 => None,
        1 => body_fields.next(),
        _ => {
            let mut flds = body_fields;

            let joint_spans: proc_macro2::Span = flds.next().span();
            for fld in flds {
                joint_spans.join(fld.span());
            }
            return Err(syn::Error::new(
                joint_spans,
                "Multiple body field definitions!",
            ));
        }
    };

    let (body_field, body_field_type) = match field_with_body_opt {
        Some(x) => {
            let field_name = x.ident.clone().unwrap();
            let field_type = x.ty.clone();

            (
                quote::quote! { self.#field_name },
                quote::quote! { #field_type },
            )
        }
        None => (quote::quote! { self }, quote::quote! { #body_type_full }),
    };

    let body_tokens = match get_body_tokens(data_type, body_field, method) {
        Ok(bdy_tkns) => bdy_tkns,
        Err(e) => {
            return Err(e);
        }
    };
    Ok((body_tokens, body_field_type))
}

fn parse_params(path: &syn::LitStr) -> Result<Vec<(String, proc_macro2::Span)>, syn::Error> {
    let binding = path.value();
    let path_str = binding.as_str();

    Ok(path_str
        .split("/")
        .filter_map(|x| {
            if x.starts_with("{") && x.ends_with("}") {
                let part = &x[1..x.len() - 1];

                Some((part.to_string(), path.span()))
            } else {
                None
            }
        })
        .collect())
}

fn field_name_exists(fields: &syn::Fields, name: &str) -> bool {
    fields
        .iter()
        .filter(|x| x.ident.as_ref().unwrap().to_string().trim() == name)
        .count()
        .eq(&1)
}

pub(crate) fn get_body_tokens(
    get_data: &syn::Ident,
    body_field: proc_macro2::TokenStream,
    method: &syn::LitStr,
) -> Result<proc_macro2::TokenStream, syn::Error> {
    Ok(match get_data.to_string().trim() {
        "Query" => {
            if ["patch", "post", "put"].contains(&method.value().as_str()) {
                return Err(syn::Error::new(
                    method.span(),
                    "Unsupported method for Query payload!",
                ));
            }
            quote::quote! {ask_nicely::request_data::RequestData::Query(&#body_field)}
        }
        "Json" => {
            if ["get", "delete"].contains(&method.value().as_str()) {
                return Err(syn::Error::new(
                    method.span(),
                    "Unsupported method for JSON payload!",
                ));
            }
            quote::quote! {ask_nicely::request_data::RequestData::Json(&#body_field)}
        }
        "Xml" => {
            if ["get", "delete"].contains(&method.value().as_str()) {
                return Err(syn::Error::new(
                    method.span(),
                    "Unsupported method for Xml payload!",
                ));
            }
            quote::quote! {ask_nicely::request_data::RequestData::Xml(&#body_field)}
        }
        "Form" => {
            if ["get", "delete"].contains(&method.value().as_str()) {
                return Err(syn::Error::new(
                    method.span(),
                    "Unsupported method for Form payload!",
                ));
            }
            quote::quote! {ask_nicely::request_data::RequestData::Form(&#body_field)}
        }
        "Binary" => {
            if ["get", "delete"].contains(&method.value().as_str()) {
                return Err(syn::Error::new(
                    method.span(),
                    "Unsupported method for Binary payload!",
                ));
            }
            quote::quote! {ask_nicely::request_data::RequestData::Binary(&#body_field)}
        }
        "None" => quote::quote! {ask_nicely::request_data::RequestData::None},
        _ => {
            return Err(syn::Error::new(
                get_data.span(),
                "Expected one of Query, Json, Xml, Form, Binary, None",
            ));
        }
    })
}

fn get_path_tokens(
    path: &syn::LitStr,
    fields: &syn::Fields,
    struct_name: &str,
) -> Result<proc_macro2::TokenStream, syn::Error> {
    let path_fields_str_span = match parse_params(path) {
        Ok(path_fields_str_span) => {
            for (name, span) in &path_fields_str_span {
                if !field_name_exists(fields, name) {
                    return Err(syn::Error::new(
                        *span,
                        format!("Field {} doesnt exist in {}.", name, struct_name),
                    ));
                }
            }
            path_fields_str_span
        }
        Err(err) => {
            return Err(err);
        }
    };

    Ok(if path_fields_str_span.is_empty() {
        quote::quote! {std::borrow::Cow::Borrowed(#path)}
    } else {
        let name_idents = path_fields_str_span
            .iter()
            .map(|x| syn::Ident::from_string(&x.0).unwrap())
            .collect::<Vec<syn::Ident>>();

        quote::quote! {
            #(
            let #name_idents = self.#name_idents;
            )*
            std::borrow::Cow::Owned(format!(#path))
        }
    })
}

fn get_method_tokens(
    method: &syn::LitStr,
) -> Result<proc_macro2::TokenStream, syn::Error> {
    
    Ok(match method.value().to_ascii_lowercase().trim() {
        "get" => quote::quote! { reqwest::Method::GET },
        "post" => quote::quote! { reqwest::Method::POST },
        "put" => quote::quote! { reqwest::Method::PUT },
        "delete" => quote::quote! { reqwest::Method::DELETE },
        "patch" => quote::quote! { reqwest::Method::PATCH },
        _ => {
            return Err(syn::Error::new(
                method.span(),
                "Expected one of get, post, put, patch, delete",
            ));
        }
    })
}

fn process_generics(generics: &syn::Generics) -> Result<syn::Generics, syn::Error> {

    if let Some(type_param) = generics.type_params().next() {
        return Err(syn::Error::new(
            type_param.span(),
            "Generic types not allowed as request objects.",
        ));
    }

    match generics.lifetimes().count() {
        0 => Ok(syn::Generics::default()),
        1 => {
            let mut punct_sect: syn::punctuated::Punctuated<syn::GenericParam, syn::token::Comma> =
                Default::default();
            punct_sect.push(syn::GenericParam::Lifetime(syn::LifetimeParam::new(
                syn::Lifetime::new("'a", proc_macro2::Span::call_site()),
            )));

            Ok(syn::Generics {
                lt_token: Some(syn::token::Lt::default()),
                params: punct_sect,
                gt_token: Some(syn::token::Gt::default()),
                where_clause: None,
            })
        }
        _ => Err(syn::Error::new(
            generics.span(),
            "Only one lifetime parameter allowed.",
        )),
    }
}

pub(crate) fn gen_request_impl(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let inpt = input.clone();
    let ast = syn::parse_macro_input!(inpt as syn::DeriveInput);
    let root_ident = &ast.ident;

    let generics = match process_generics(&ast.generics) {
        Ok(genercs) => genercs,
        Err(err) => {
            return err.into_compile_error().into();
        }
    };

    let mut root_type_with_generics = quote::quote! { #root_ident };

    root_type_with_generics.extend(generics.to_token_stream());

    let syn::DataStruct { fields, .. } = match &ast.data {
        syn::Data::Struct(data_struct) => data_struct,
        _ => {
            return syn::Error::new(ast.ident.span(), "Struct is not a data struct")
                .to_compile_error()
                .into();
        }
    };

    let implementer = &ast.ident;

    let RequestAttr {
        authed,
        response,
        method,
        path,
        get_data,
    } = match RequestAttr::from_derive_input(&ast) {
        Ok(attr) => attr,
        Err(e) => {
            return e.write_errors().into();
        }
    };

    let path_tokens = match get_path_tokens(&path, fields, implementer.to_string().trim()) {
        Ok(tokens) => tokens,
        Err(err) => {
            return err.into_compile_error().into();
        }
    };

    let method_tokens = match get_method_tokens(&method) {
        Ok(method) => method,
        Err(e) => {
            return e.to_compile_error().into();
        }
    };

    let (body_tokens, body_type) = match get_body_tokens_with_payload_type(
        fields,
        &get_data,
        &method,
        root_type_with_generics,
    ) {
        Ok(pair) => pair,
        Err(e) => {
            return e.into_compile_error().into();
        }
    };

    quote::quote! {
        impl<'a> ask_nicely::request::Request<'a> for #implementer #generics {
            type Response = #response;
            type ReqObj = #body_type;

            const AUTHED: bool = #authed;
            const METHOD: reqwest::Method = #method_tokens;

            fn get_path(&'a self) -> std::borrow::Cow<'a, str> {
                #path_tokens
            }

            fn get_data(&'a self) -> ask_nicely::request_data::RequestData<'_, Self::ReqObj> {
                #body_tokens
            }
        }
    }
    .into()
}
