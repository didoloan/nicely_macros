use quote::ToTokens;
use syn::spanned::Spanned;
use darling::{FromDeriveInput, FromField};

#[derive(Debug, darling::FromDeriveInput)]
#[darling(attributes(request))]
pub(crate) struct RequestAttr {
    pub authed: bool,
    pub response: syn::Ident,
    pub method: syn::LitStr,
    pub path: syn::LitStr,
    pub get_data: syn::Ident,
}

#[derive(Debug, darling::FromField)]
#[darling(attributes(request))]
pub(crate) struct RequestFieldBodyAttr {
    pub body: bool,
}

#[derive(Debug, darling::FromField)]
#[darling(attributes(request))]
pub(crate) struct RequestFieldPathAttr {
    pub path_var: u8,
}

pub(crate) fn ensure_basic_type(fld_type: &syn::Type) -> bool {
    let types = "u8u16u32u64u128usizei8i16i32i64i128isizef32f64f128String&str&String";
    
    if types.contains(fld_type.to_token_stream().to_string().trim()) {
        return true;
    }
    false
}

pub(crate) fn count_receivers(text: &syn::LitStr) -> usize {
    let mut count = 0;
    let text = text.value();

    let text_len = text.len();
    let substring = "{}";
    let substring_len = 2;

    if substring_len == 0 {
        return 0; // Or handle as you see fit (e.g., return text_len + 1)
    }

    if substring_len > text_len {
        return 0; // Substring cannot occur if it's longer than the text
    }

    for i in 0..=text_len - substring_len {
        // Iterate through possible start positions
        let slce = &text[i..i + substring_len];
        if slce == substring {
            count += 1;
        }
    }
    count
}

pub(crate) fn get_body_tokens(
    get_data: &syn::Ident,
    body_field: proc_macro2::TokenStream,
) -> Result<proc_macro2::TokenStream, syn::Error> {
    Ok(match get_data.to_string().trim() {
        "Query" => quote::quote! {ask_nicely::request_data::RequestData::Query(&#body_field)},
        "Json" => quote::quote! {ask_nicely::request_data::RequestData::Json(&#body_field)},
        "Xml" => quote::quote! {ask_nicely::request_data::RequestData::Xml(&#body_field)},
        "Form" => quote::quote! {ask_nicely::request_data::RequestData::Form(&#body_field)},
        "Binary" => quote::quote! {ask_nicely::request_data::RequestData::Binary(&#body_field)},
        "None" => quote::quote! {ask_nicely::request_data::RequestData::None},
        _ => {
            return Err(syn::Error::new(
                get_data.span(),
                "Expected one of Query, Json, Xml, Form, Binary, None",
            ));
        }
    })
}

pub(crate) fn get_method_tokens(method: &syn::LitStr) -> Result<proc_macro2::TokenStream, syn::Error> {
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

pub(crate) fn gen_request_impl(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let inpt = input.clone();
    let ast = syn::parse_macro_input!(inpt as syn::DeriveInput);

    let root_ident = &ast.ident;

    let generics = ast.generics.clone();

    let mut type_params = generics.type_params();
    let ltimes = generics.lifetimes();

    while let Some(tparam) = type_params.next() {
        return syn::Error::new(tparam.span(), "Typed generics not allowed on request objects. Only one lifetime allowed.")
        .into_compile_error().into();
    }

    let generics = match ltimes.count() {
        0 => syn::Generics::default(),
        1 => {
            let mut punct_sect:syn::punctuated::Punctuated<syn::GenericParam, syn::token::Comma> = Default::default();
            punct_sect.push(syn::GenericParam::Lifetime(syn::LifetimeParam::new(syn::Lifetime::new("'a", proc_macro2::Span::call_site()))));
            
            syn::Generics {
                lt_token: Some(syn::token::Lt::default()),
                params: punct_sect,
                gt_token: Some(syn::token::Gt::default()),
                where_clause: None
            }
        },
        _ => {
            return syn::Error::new(generics.span(), "Only one lifetime parameter allowed.")
            .into_compile_error().into();
        }
    };

    let mut root_type = quote::quote! { #root_ident };

    root_type.extend(generics.to_token_stream());

    println!("{}", root_type.to_string());

    let syn::DataStruct { fields, .. } = match &ast.data {
        syn::Data::Struct(data_struct) => data_struct,
        _ => {
            return syn::Error::new(ast.ident.span(), "Struct is not a data struct")
                .to_compile_error()
                .into();
        }
    };

    let (body_field, body_field_type) = match fields
        .iter()
        .filter_map(|f| {
            let field_req_attr_opt = RequestFieldBodyAttr::from_field(f).ok();

            println!("{:?}", field_req_attr_opt);

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
        })
        .next()
    {
        Some(x) => {
            println!("{:?}", x);
            let field_name = x.ident.clone().unwrap();
            let field_type = x.ty.clone();
            println!("{:?}\n{:?}", field_name, field_type);
            (
                quote::quote! { self.#field_name },
                quote::quote! { #field_type },
            )
        }
        None => (quote::quote! {self}, quote::quote! {#root_type}),
    };

    let mut path_fields = fields
        .iter()
        .filter_map(|f| {
            RequestFieldPathAttr::from_field(f)
                .map(|x| ((f.ident.as_ref().unwrap(), &f.ty), x))
                .ok()
        })
        .map(|x| (x.1.path_var, x.0 .0, x.0 .1))
        // .filter(|(_, _, z)| ensure_basic_type(z))
        .map(|x| (x.0, x.1))
        .collect::<Vec<(u8, &syn::Ident)>>();

    path_fields.sort_by(|a, b| a.0.cmp(&b.0));

    let path_fields = path_fields
        .iter()
        .map(|x| x.1)
        .collect::<Vec<&syn::Ident>>();

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

    let path_tokens = if path_fields.is_empty() {
        quote::quote! {std::borrow::Cow::Borrowed(#path)}
    } else {
        match count_receivers(&path).eq(&path_fields.len()) {
            true => {
                quote::quote! { 
                    std::borrow::Cow::Owned(format!(#path, #( self.#path_fields , )*)) 
                }
            }
            false => quote::quote! { std::borrow::Cow::Borrowed(#path) },
        }
    };

    let method = match get_method_tokens(&method) {
        Ok(method) => method,
        Err(e) => {
            return e.to_compile_error().into();
        }
    };

    let body_tokens = match get_body_tokens(&get_data, body_field) {
        Ok(bdy_tkns) => bdy_tkns,
        Err(e) => {
            return e.to_compile_error().into();
        }
    };

    let output = quote::quote! {
        impl<'a> ask_nicely::request::Request<'a> for #implementer #generics {
            type Response = #response;
            type ReqObj = #body_field_type;

            const AUTHED: bool = #authed;
            const METHOD: reqwest::Method = #method;

            fn get_path(&'a self) -> std::borrow::Cow<'a, str> {
                #path_tokens
            }

            fn get_data(&'a self) -> ask_nicely::request_data::RequestData<'_, Self::ReqObj> {
                #body_tokens
            }
        }
    };

    output.into()
}
