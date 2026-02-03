use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, Data, DeriveInput, Fields, GenericArgument, PathArguments, Type, TypePath,
};

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

fn is_as_enum_field(field: &syn::Field) -> bool {
    field
        .attrs
        .iter()
        .any(|attr| attr.path().is_ident("as_enum"))
}

/// Extract aliases from #[alias("name1", "name2", ...)] attribute
fn extract_aliases(field: &syn::Field) -> Vec<String> {
    let mut aliases = Vec::new();

    for attr in &field.attrs {
        if attr.path().is_ident("alias") {
            // Parse the alias attribute
            // Expected format: #[alias("name1")] or #[alias("name1", "name2")]
            let result: Result<Vec<String>, _> =
                attr.parse_args_with(|input: syn::parse::ParseStream| {
                    // input is already the content inside the parentheses
                    let mut names = Vec::new();
                    loop {
                        if input.is_empty() {
                            break;
                        }

                        let lit: syn::LitStr = input.parse()?;
                        names.push(lit.value());

                        if !input.is_empty() {
                            input.parse::<syn::Token![,]>()?;
                        }
                    }
                    Ok(names)
                });

            if let Ok(found_aliases) = result {
                aliases.extend(found_aliases);
            }
        }
    }

    aliases
}

fn extract_type_name(ty: &Type) -> Option<syn::Ident> {
    match ty {
        Type::Path(TypePath { path, .. }) => path.get_ident().cloned(),
        _ => None,
    }
}

fn extract_option_inner_type(ty: &Type) -> Option<syn::Ident> {
    // Check if this is an Option<T> type
    match ty {
        Type::Path(TypePath { path, .. }) => {
            // Check if the path ends with "Option"
            if let Some(segment) = path.segments.last() {
                if segment.ident == "Option" {
                    // Extract the inner type from the angle brackets
                    match &segment.arguments {
                        PathArguments::AngleBracketed(args) => {
                            if let Some(GenericArgument::Type(inner_ty)) = args.args.first() {
                                return extract_type_name(&inner_ty);
                            }
                        }
                        _ => {}
                    }
                }
            }
            None
        }
        _ => None,
    }
}

fn is_option_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => {
            if let Some(segment) = path.segments.last() {
                segment.ident == "Option"
            } else {
                false
            }
        }
        _ => false,
    }
}

fn extract_vec_inner_type(ty: &Type) -> Option<syn::Ident> {
    // Check if this is a Vec<T> type
    match ty {
        Type::Path(TypePath { path, .. }) => {
            // Check if the path ends with "Vec"
            if let Some(segment) = path.segments.last() {
                if segment.ident == "Vec" {
                    // Extract the inner type from the angle brackets
                    match &segment.arguments {
                        PathArguments::AngleBracketed(args) => {
                            if let Some(GenericArgument::Type(inner_ty)) = args.args.first() {
                                return extract_type_name(&inner_ty);
                            }
                        }
                        _ => {}
                    }
                }
            }
            None
        }
        _ => None,
    }
}

fn is_vec_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => {
            if let Some(segment) = path.segments.last() {
                segment.ident == "Vec"
            } else {
                false
            }
        }
        _ => false,
    }
}

fn is_enum_tagged_field(field: &syn::Field) -> bool {
    field
        .attrs
        .iter()
        .any(|attr| attr.path().is_ident("enum_tagged"))
}

fn extract_tag_key(field: &syn::Field) -> Option<String> {
    for attr in &field.attrs {
        if attr.path().is_ident("enum_tagged") {
            let result: Result<String, _> =
                attr.parse_args_with(|input: syn::parse::ParseStream| {
                    let lit: syn::LitStr = input.parse()?;
                    Ok(lit.value())
                });
            if let Ok(key) = result {
                return Some(key);
            }
        }
    }
    None
}

#[proc_macro_attribute]
pub fn make_partial_enum(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    let enum_name = &input.ident;
    let partial_name = format_ident!("Partial{}", enum_name);
    let generics = &input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let variants = match &input.data {
        Data::Enum(data) => &data.variants,
        _ => panic!("make_partial_enum only supports enums"),
    };

    // Generate partial enum variants
    let partial_variants: Vec<_> = variants
        .iter()
        .map(|v| {
            let variant_name = &v.ident;
            match &v.fields {
                Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                    let inner_ty = &fields.unnamed.first().unwrap().ty;
                    let inner_type_name = extract_type_name(inner_ty).unwrap();
                    let partial_inner_type = format_ident!("Partial{}", inner_type_name);
                    quote! {
                        #variant_name(#partial_inner_type)
                    }
                }
                _ => panic!("make_partial_enum only supports single unnamed field variants"),
            }
        })
        .collect();

    // Collect errors implementation
    let collect_errors_variants: Vec<_> = variants
        .iter()
        .map(|v| {
            let variant_name = &v.ident;
            quote! {
                #partial_name::#variant_name(inner) => inner.collect_errors(errors),
            }
        })
        .collect();

    // Can concrete implementation
    let can_concrete_variants: Vec<_> = variants
        .iter()
        .map(|v| {
            let variant_name = &v.ident;
            quote! {
                #partial_name::#variant_name(inner) => inner.can_concrete(),
            }
        })
        .collect();

    // To concrete implementation
    let to_concrete_variants: Vec<_> = variants
        .iter()
        .map(|v| {
            let variant_name = &v.ident;
            let _inner_type_name = match &v.fields {
                Fields::Unnamed(fields) => {
                    extract_type_name(&fields.unnamed.first().unwrap().ty).unwrap()
                }
                _ => unreachable!(),
            };
            quote! {
                #partial_name::#variant_name(inner) => {
                    inner.to_concrete().map(#enum_name::#variant_name)
                }
            }
        })
        .collect();

    let variant_names: Vec<_> = variants
        .iter()
        .map(|v| {
            let name = &v.ident;
            name.to_string()
        })
        .collect();

    // Generate deserialize_variant match arms
    let deserialize_variant_arms: Vec<_> = variants
        .iter()
        .map(|v| {
            let variant_name = &v.ident;
            let variant_name_str = variant_name.to_string();
            match &v.fields {
                Fields::Unnamed(fields) => {
                    let inner_ty = &fields.unnamed.first().unwrap().ty;
                    let inner_type_name = extract_type_name(inner_ty).unwrap();
                    let partial_inner_type = format_ident!("Partial{}", inner_type_name);
                    quote! {
                        #variant_name_str => {
                            let result: ::toml_pretty_deser::TomlValue<#partial_inner_type> = ::toml_pretty_deser::deserialize_nested(
                                item,
                                &(0..0),
                                errors,
                                mode,
                                true,
                                &[tag_field],
                            );
                            result.value.map(#partial_name::#variant_name)
                        }
                    }
                }
                _ => unreachable!(),
            }
        })
        .collect();

    let expanded = quote! {
        #input

        #[derive(Debug, Clone)]
        #generics
        enum #partial_name #ty_generics #where_clause {
            #(#partial_variants,)*
        }

        impl #impl_generics #partial_name #ty_generics #where_clause {
            /// Deserialize a specific variant by name from a TOML item
            pub fn deserialize_variant(
                variant_name: &str,
                item: &::toml_edit::Item,
                errors: &std::rc::Rc<std::cell::RefCell<Vec<::toml_pretty_deser::AnnotatedError>>>,
                mode: ::toml_pretty_deser::FieldMatchMode,
                tag_field: &str
            ) -> Option<Self> {
                match variant_name {
                    #(#deserialize_variant_arms,)*
                    _ => None,
                }
            }
        }

        impl #impl_generics StringNamedEnum for #partial_name #ty_generics #where_clause {
            fn all_variant_names() -> &'static [&'static str] {
                &[#(#variant_names),*]
            }

            fn from_str(s: &str) -> Option<Self> {
                // This is used by AsTaggedEnum for variant matching
                // The actual deserialization happens through deserialize_variant
                None
            }
        }

        impl #impl_generics ToConcrete<#enum_name #ty_generics> for #partial_name #ty_generics #where_clause {
            fn collect_errors(&self, errors: &std::rc::Rc<std::cell::RefCell<Vec<AnnotatedError>>>) {
                match self {
                    #(#collect_errors_variants)*
                }
            }

            fn to_concrete(self) -> Option<#enum_name #ty_generics> {
                match self {
                    #(#to_concrete_variants,)*
                }
            }
        }

        impl #impl_generics FromTomlTable<()> for #partial_name #ty_generics #where_clause {
            fn can_concrete(&self) -> bool {
                match self {
                    #(#can_concrete_variants)*
                }
            }

            fn from_toml_table(_helper: &mut ::toml_pretty_deser::TomlHelper<'_>, _partial: &()) -> Self {
                // Note: This should not be called directly for tagged enums.
                // Tagged enums should use AsTaggedEnum::as_tagged_enum instead.
                panic!("FromTomlTable<()> should not be called directly on tagged enums. Use AsTaggedEnum::as_tagged_enum instead.");
            }
        }

        impl #impl_generics ::toml_pretty_deser::VerifyFromToml<()> for #partial_name #ty_generics #where_clause {
            fn verify(self, _helper: &mut ::toml_pretty_deser::TomlHelper<'_>, _partial: &()) -> Self {
                self
            }
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro_attribute]
pub fn make_partial(attr: TokenStream, item: TokenStream) -> TokenStream {
    // Parse the boolean argument (default to true)
    let generate_verify = if attr.is_empty() {
        true
    } else {
        let attr_str = attr.to_string();
        match attr_str.as_str() {
            "true" => true,
            "false" => false,
            _ => panic!("make_partial expects 'true' or 'false', got: {}", attr_str),
        }
    };

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

    // Create a version of the input struct with #[nested], #[as_enum], #[enum_tagged(...)], and #[alias(...)] attributes stripped
    let mut cleaned_input = input.clone();
    if let Data::Struct(ref mut data) = cleaned_input.data {
        if let Fields::Named(ref mut fields) = data.fields {
            for field in fields.named.iter_mut() {
                field.attrs.retain(|attr| {
                    !attr.path().is_ident("nested")
                        && !attr.path().is_ident("as_enum")
                        && !attr.path().is_ident("enum_tagged")
                        && !attr.path().is_ident("alias")
                });
            }
        }
    }

    // Validate nested and enum_tagged fields first
    for f in fields.iter() {
        if is_nested_field(f) {
            let ty = &f.ty;
            if is_option_type(ty) {
                if extract_option_inner_type(ty).is_none() {
                    panic!("nested attribute on Option field requires a simple inner type name");
                }
            } else if is_vec_type(ty) {
                if extract_vec_inner_type(ty).is_none() {
                    panic!("nested attribute on Vec field requires a simple inner type name");
                }
            } else if extract_type_name(ty).is_none() {
                panic!("nested attribute requires a simple type name");
            }
        }
        if is_enum_tagged_field(f) {
            let ty = &f.ty;
            if extract_type_name(ty).is_none() {
                panic!("enum_tagged attribute requires a simple type name");
            }
            if extract_tag_key(f).is_none() {
                panic!("enum_tagged attribute requires a tag key: #[enum_tagged(\"key\")]");
            }
        }
    }

    let partial_fields: Vec<_> = fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            let ty = &f.ty;

            if is_nested_field(f) {
                // Check if this is Option<InnerType>, Vec<InnerType>, or just InnerType
                if is_option_type(ty) {
                    // For Option<Nested> fields, use Option<PartialType>
                    let inner_type_name = extract_option_inner_type(ty).unwrap();
                    let partial_type = format_ident!("Partial{}", inner_type_name);
                    quote! {
                        #name: TomlValue<Option<#partial_type>>
                    }
                } else if is_vec_type(ty) {
                    // For Vec<Nested> fields, use Vec<PartialType>
                    let inner_type_name = extract_vec_inner_type(ty).unwrap();
                    let partial_type = format_ident!("Partial{}", inner_type_name);
                    quote! {
                        #name: TomlValue<Vec<#partial_type>>
                    }
                } else {
                    // For regular nested fields, use Partial{Type}
                    let type_name = extract_type_name(ty).unwrap();
                    let partial_type = format_ident!("Partial{}", type_name);
                    quote! {
                        #name: TomlValue<#partial_type>
                    }
                }
            } else if is_enum_tagged_field(f) {
                // For enum_tagged fields, use Partial{EnumType}
                let type_name = extract_type_name(ty).unwrap();
                let partial_type = format_ident!("Partial{}", type_name);
                quote! {
                    #name: TomlValue<#partial_type>
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
                // For nested fields (including Option<Nested> and Vec<Nested>), recursively collect errors
                if is_option_type(&f.ty) {
                    // For Option<Nested>, value is Option<Option<PartialType>>
                    quote! {
                        if let Some(Some(ref partial)) = self.#name.value {
                            partial.collect_errors(errors);
                        }
                    }
                } else if is_vec_type(&f.ty) {
                    // For Vec<Nested>, value is Option<Vec<PartialType>>
                    quote! {
                        if let Some(ref partials) = self.#name.value {
                            for partial in partials.iter() {
                                partial.collect_errors(errors);
                            }
                        }
                    }
                } else {
                    quote! {
                        if let Some(ref partial) = self.#name.value {
                            partial.collect_errors(errors);
                        }
                    }
                }
            } else if is_enum_tagged_field(f) {
                // For enum_tagged fields, recursively collect errors from the partial enum
                quote! {
                    if let Some(ref partial) = self.#name.value {
                        partial.collect_errors(errors);
                    } else {
                        self.#name.register_error(errors);
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
                // For Option<PartialType>, it's concrete if Some and inner can_concrete, or if None
                if is_option_type(&f.ty) {
                    quote! {
                        self.#name.value.as_ref().map(|opt| {
                            opt.as_ref().map(|p| p.can_concrete()).unwrap_or(true)
                        }).unwrap_or(false)
                    }
                } else if is_vec_type(&f.ty) {
                    // For Vec<Nested>, all items must be concrete
                    quote! {
                        self.#name.value.as_ref().map(|vec| {
                            vec.iter().all(|p| p.can_concrete())
                        }).unwrap_or(false)
                    }
                } else {
                    quote! {
                        self.#name.value.as_ref().map(|p| p.can_concrete()).unwrap_or(false)
                    }
                }
            } else if is_enum_tagged_field(f) {
                // For enum_tagged fields, check if the partial enum can become concrete
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
                if is_option_type(&f.ty) {
                    // For Option<Nested>, convert Option<Partial> to Option<Concrete>
                    // self.#name.value is Option<Option<PartialNested>>
                    // We need to flatten it and then convert
                    quote! {
                        #name: self.#name.value.flatten().and_then(|p| p.to_concrete())
                    }
                } else if is_vec_type(&f.ty) {
                    // For Vec<Nested>, convert each partial in Vec<Partial> to Vec<Concrete>
                    quote! {
                        #name: self.#name.value.map(|vec| {
                            vec.into_iter().filter_map(|p| p.to_concrete()).collect()
                        }).unwrap()
                    }
                } else {
                    // For regular nested fields, convert Partial to Concrete
                    quote! {
                        #name: self.#name.value.and_then(|p| p.to_concrete()).unwrap()
                    }
                }
            } else if is_enum_tagged_field(f) {
                // For enum_tagged fields, convert PartialEnum to ConcreteEnum
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

    let from_toml_table_fields: Vec<_> = fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            let name_str = name.as_ref().unwrap().to_string();
            let aliases = extract_aliases(f);
            
            if is_nested_field(f) {
                // For nested fields, we need to pass mode to as_nested
                quote! {
                    #name: helper.get_with_aliases(#name_str, vec![]).as_nested(&helper.errors, helper.match_mode)
                }
            } else if is_enum_tagged_field(f) {
                // For enum_tagged fields, use as_tagged_enum with the tag key and deserialize function
                let tag_key = extract_tag_key(f).unwrap();
                let type_name = extract_type_name(&f.ty).unwrap();
                let partial_type = format_ident!("Partial{}", type_name);
                quote! {
                    #name: helper.get_with_aliases(#name_str, vec![]).as_tagged_enum(#tag_key, &helper.errors, helper.match_mode, #partial_type::deserialize_variant)
                }
            } else if is_as_enum_field(f) {
                // For enum fields, use aliases if present
                if aliases.is_empty() {
                    quote! {
                        #name: helper.get(#name_str).as_enum()
                    }
                } else {
                    quote! {
                        #name: helper.get_with_aliases(#name_str, vec![]).as_enum()
                    }
                }
            } else {
                // For regular fields, use aliases if present
                if aliases.is_empty() {
                    quote! {
                        #name: helper.get(#name_str)
                    }
                } else {
                    quote! {
                        #name: helper.get_with_aliases(#name_str, vec![#(#aliases),*])
                    }
                }
            }
        })
        .collect();

    // Conditionally generate VerifyFromToml impl
    let generate_verify_impl = if generate_verify {
        quote! {
            impl #impl_generics VerifyFromToml<()> for #partial_name #ty_generics #where_clause {
                fn verify(self, _helper: &mut TomlHelper<'_>, _partial: &()) -> Self {
                    self
                }
            }
        }
    } else {
        quote! {}
    };

    let expanded = quote! {
        #cleaned_input

        #[derive(Debug, Clone)]
        struct #partial_name #generics #where_clause {
            #(#partial_fields,)*
        }

        impl #impl_generics ToConcrete<#struct_name #ty_generics> for #partial_name #ty_generics #where_clause {
            fn collect_errors(&self, errors: &Rc<RefCell<Vec<AnnotatedError>>>) {
            dbg!(&self);
                #(#collect_errors_fields;)*
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

        impl #impl_generics FromTomlTable<()> for #partial_name #ty_generics #where_clause {
            fn can_concrete(&self) -> bool {
                #(#can_concrete_fields)&&*
            }
            fn from_toml_table(helper: &mut TomlHelper<'_>, _partial: &()) -> Self {
                #partial_name {
                    #(#from_toml_table_fields,)*
                }
            }
        }

        #generate_verify_impl
    };

    TokenStream::from(expanded)
}
