use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Fields, Type, TypePath};

#[proc_macro_derive(StringNamedEnum)]
pub fn derive_string_named_enum(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let enum_name = &input.ident;
    let generics = &input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let variants = match &input.data {
        Data::Enum(data) => &data.variants,
        _ => panic!("StringNamedEnum can only be derived for enums"),
    };

    let variant_names: Vec<_> = variants
        .iter()
        .map(|v| {
            let name = &v.ident;
            name.to_string()
        })
        .collect();

    let from_str_arms = variants.iter().map(|v| {
        let name = &v.ident;
        let name_str = name.to_string();
        quote! {
            #name_str => Some(#enum_name::#name)
        }
    });

    let expanded = quote! {
        impl #impl_generics StringNamedEnum for #enum_name #ty_generics #where_clause {
            fn all_variant_names() -> &'static [&'static str] {
                &[#(#variant_names),*]
            }

            fn from_str(s: &str) -> Option<Self> {
                match s {
                    #(#from_str_arms,)*
                    _ => None,
                }
            }
        }
    };

    TokenStream::from(expanded)
}

fn is_nested_field(field: &syn::Field) -> bool {
    field
        .attrs
        .iter()
        .any(|attr| attr.path().is_ident("nested"))
}

fn extract_type_name(ty: &Type) -> Option<syn::Ident> {
    match ty {
        Type::Path(TypePath { path, .. }) => path.get_ident().cloned(),
        _ => None,
    }
}

#[proc_macro_attribute]
pub fn make_partial(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);

    let struct_name = &input.ident;
    let partial_name = format_ident!("Partial{}", struct_name);
    let generics = &input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let fields = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => &fields.named,
            _ => panic!("make_partial only supports structs with named fields"),
        },
        _ => panic!("make_partial only supports structs"),
    };

    // Create a version of the input struct with #[nested] attributes stripped
    let mut cleaned_input = input.clone();
    if let Data::Struct(ref mut data) = cleaned_input.data {
        if let Fields::Named(ref mut fields) = data.fields {
            for field in fields.named.iter_mut() {
                field.attrs.retain(|attr| !attr.path().is_ident("nested"));
            }
        }
    }

    let partial_fields: Vec<_> = fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            let ty = &f.ty;

            if is_nested_field(f) {
                // For nested fields, use Partial{Type}
                if let Some(type_name) = extract_type_name(ty) {
                    let partial_type = format_ident!("Partial{}", type_name);
                    quote! {
                        #name: TomlValue<#partial_type>
                    }
                } else {
                    panic!("nested attribute requires a simple type name");
                }
            } else {
                quote! {
                    #name: TomlValue<#ty>
                }
            }
        })
        .collect();

    let collect_errors_fields: Vec<_> = fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            if is_nested_field(f) {
                // For nested fields, recursively collect errors from the partial
                quote! {
                    if let Some(ref partial) = self.#name.value {
                        partial.collect_errors(errors);
                    }
                }
            } else {
                quote! {
                    self.#name.register_error(errors)
                }
            }
        })
        .collect();

    let can_concrete_fields: Vec<_> = fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            if is_nested_field(f) {
                // For nested fields, check if the partial can become concrete
                quote! {
                    self.#name.value.as_ref().map(|p| p.can_concrete()).unwrap_or(false)
                }
            } else {
                quote! {
                    self.#name.has_value()
                }
            }
        })
        .collect();

    let to_concrete_fields: Vec<_> = fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            if is_nested_field(f) {
                // For nested fields, recursively convert the partial to concrete
                quote! {
                    #name: self.#name.value.and_then(|p| p.to_concrete()).unwrap()
                }
            } else {
                quote! {
                    #name: self.#name.unwrap()
                }
            }
        })
        .collect();

    let expanded = quote! {
        #cleaned_input

        #[derive(Debug)]
        struct #partial_name #generics #where_clause {
            #(#partial_fields,)*
        }

        impl #impl_generics ToConcrete<#struct_name #ty_generics> for #partial_name #ty_generics #where_clause {
            fn collect_errors(&self, errors: &Rc<RefCell<Vec<AnnotatedError>>>) {
                #(#collect_errors_fields;)*
            }

            fn can_concrete(&self) -> bool {
                #(#can_concrete_fields)&&*
            }

            fn to_concrete(self) -> Option<#struct_name #ty_generics> {
                Some(#struct_name {
                    #(#to_concrete_fields,)*
                })
            }
        }

        impl #impl_generics PartialEq for #partial_name #ty_generics #where_clause {
            fn eq(&self, other: &Self) -> bool {
                true
            }
        }
    };

    TokenStream::from(expanded)
}
