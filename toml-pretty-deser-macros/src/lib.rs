use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{
    Data, DeriveInput, Fields, GenericArgument, PathArguments, Type, TypePath, parse_macro_input,
};

/// # Tag an enum for use with `deserialize`
/// ```ignore
/// #[tpd_make_enum]
/// #[derive(Debug, Clone)]
/// enum Color {
///     Red,
///     Green,
///     Blue,
/// }
///
/// #[tpd_make_partial]
/// struct Config {
///     color: Color,  // Just works!
///     colors: Vec<Color>,  // Also works!
///     maybe_color: Option<Color>,  // Yep!
/// }
/// ```
#[proc_macro_attribute]
pub fn tpd_make_enum(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    let enum_name = &input.ident;
    let generics = &input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let variants = match &input.data {
        Data::Enum(data) => &data.variants,
        _ => panic!("tpd_make_enum can only be applied to enums"),
    };

    // Validate that all variants are unit variants (no fields)
    for v in variants.iter() {
        match &v.fields {
            Fields::Unit => {}
            _ => panic!(
                "tpd_make_enum only supports unit variants (no fields). Variant '{}' has fields.",
                v.ident
            ),
        }
    }

    let variant_names: Vec<_> = variants.iter().map(|v| v.ident.to_string()).collect();

    let from_str_arms = variants.iter().map(|v| {
        let name = &v.ident;
        let name_str = name.to_string();
        quote! {
            #name_str => ::std::option::Option::Some(#enum_name::#name)
        }
    });

    let expanded = quote! {
        #input

        impl #impl_generics ::toml_pretty_deser::StringNamedEnum for #enum_name #ty_generics #where_clause {
            fn all_variant_names() -> &'static [&'static str] {
                &[#(#variant_names),*]
           }

            fn from_str(s: &str) -> ::std::option::Option<Self> {
                match s {
                    #(#from_str_arms,)*
                    _ => ::std::option::Option::None,
                }
            }
        }

        impl #impl_generics ::toml_pretty_deser::FromTomlItem for #enum_name #ty_generics #where_clause {
            fn from_toml_item(
                item: &::toml_edit::Item,
                parent_span: ::std::ops::Range<usize>,
                col: &TomlCollector,
            ) -> ::toml_pretty_deser::TomlValue<Self> {
                match item {
                    ::toml_edit::Item::None => ::toml_pretty_deser::TomlValue::new_empty_missing(parent_span),
                    ::toml_edit::Item::Value(::toml_edit::Value::String(formatted)) => {
                        let s = formatted.value();
                        let span = formatted.span().unwrap_or(parent_span.clone());
                        match <#enum_name as ::toml_pretty_deser::StringNamedEnum>::from_str(s) {
                            ::std::option::Option::Some(enum_val) => {
                                ::toml_pretty_deser::TomlValue::new_ok(enum_val, span)
                            }
                            ::std::option::Option::None => {
                                let help = ::toml_pretty_deser::suggest_enum_alternatives::<#enum_name>(s);
                                let res = ::toml_pretty_deser::TomlValue::new_validation_failed(
                                    span,
                                    "Invalid enum variant.".to_string(),
                                    ::std::option::Option::Some(help),
                                );
                                res.register_error(&col.errors);
                                res
                            }
                        }
                    }
                    other => ::toml_pretty_deser::TomlValue::new_wrong_type(other, parent_span, "string"),
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

fn is_defaulted_field(field: &syn::Field) -> bool {
    field
        .attrs
        .iter()
        .any(|attr| attr.path().is_ident("tpd_default_in_verify"))
}

/// Extract aliases from #[alias("name1", "name2", ...)] attribute
fn extract_aliases(field: &syn::Field) -> Vec<String> {
    let mut aliases = Vec::new();

    for attr in &field.attrs {
        if attr.path().is_ident("tpd_alias") {
            // Parse the alias attribute
            // Expected format: #[alias(name1)] or #[alias(name1, name2)]
            // Also accepts string literals for reserved keywords: #[alias("type")]
            let result: Result<Vec<String>, _> =
                attr.parse_args_with(|input: syn::parse::ParseStream| {
                    let mut names = Vec::new();
                    loop {
                        if input.is_empty() {
                            break;
                        }

                        // Try to parse as string literal first (for reserved keywords like "type")
                        if input.peek(syn::LitStr) {
                            let lit: syn::LitStr = input.parse()?;
                            names.push(lit.value());
                        } else {
                            // Otherwise parse as identifier
                            let ident: syn::Ident = input.parse()?;
                            names.push(ident.to_string());
                        }

                        if !input.is_empty() {
                            input.parse::<syn::Token![,]>()?;
                        }
                    }
                    Ok(names)
                });

            match result {
                Ok(found_aliases) => aliases.extend(found_aliases),
                Err(e) => panic!("{}", e),
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

/// Check if a type is IndexMap<String, T>
fn is_indexmap_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => {
            if let Some(segment) = path.segments.last() {
                segment.ident == "IndexMap"
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Check if a type is Option<IndexMap<String, T>>
fn is_option_indexmap_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => {
            if let Some(segment) = path.segments.last() {
                if segment.ident == "Option" {
                    if let PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(GenericArgument::Type(inner_ty)) = args.args.first() {
                            return is_indexmap_type(&inner_ty);
                        }
                    }
                }
            }
            false
        }
        _ => false,
    }
}

/// Extract the value type from IndexMap<String, T> - returns the full Type
fn extract_indexmap_value_type(ty: &Type) -> Option<Type> {
    match ty {
        Type::Path(TypePath { path, .. }) => {
            if let Some(segment) = path.segments.last() {
                if segment.ident == "IndexMap" {
                    if let PathArguments::AngleBracketed(args) = &segment.arguments {
                        // IndexMap<String, T> has two type arguments
                        let mut iter = args.args.iter();
                        iter.next(); // Skip String (the key type)
                        if let Some(GenericArgument::Type(value_ty)) = iter.next() {
                            return Some(value_ty.clone());
                        }
                    }
                }
            }
            None
        }
        _ => None,
    }
}

/// Extract IndexMap value type from Option<IndexMap<String, T>>
fn extract_option_indexmap_value_type(ty: &Type) -> Option<Type> {
    match ty {
        Type::Path(TypePath { path, .. }) => {
            if let Some(segment) = path.segments.last() {
                if segment.ident == "Option" {
                    if let PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(GenericArgument::Type(inner_ty)) = args.args.first() {
                            return extract_indexmap_value_type(&inner_ty);
                        }
                    }
                }
            }
            None
        }
        _ => None,
    }
}

/// Check if type is a Vec type and extract its inner type
fn extract_vec_inner_full_type(ty: &Type) -> Option<Type> {
    match ty {
        Type::Path(TypePath { path, .. }) => {
            if let Some(segment) = path.segments.last() {
                if segment.ident == "Vec" {
                    if let PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(GenericArgument::Type(inner_ty)) = args.args.first() {
                            return Some(inner_ty.clone());
                        }
                    }
                }
            }
            None
        }
        _ => None,
    }
}

/// Information about a IndexMap field type
#[derive(Clone)]
enum IndexMapValueKind {
    /// Primitive type like u8, i32, String, etc.
    Primitive,
    /// Nested struct type (needs #[nested] or similar)
    Nested(syn::Ident),
    /// Tagged enum type (needs #[enum_tagged])
    TaggedEnum(syn::Ident),
    /// Vec of primitive
    VecPrimitive,
    /// Vec of nested struct
    VecNested(syn::Ident),
    /// Vec of tagged enum
    VecTaggedEnum(syn::Ident),
}

/// Analyze the value type of a IndexMap and determine what kind it is
fn analyze_indexmap_value_type(value_ty: &Type, field: &syn::Field) -> IndexMapValueKind {
    let is_nested = is_nested_field(field);
    let is_enum_tagged = is_enum_tagged_field(field);

    // Check if value type is Vec<T>
    if let Some(vec_inner) = extract_vec_inner_full_type(value_ty) {
        // It's a Vec<T>, now determine what T is
        if is_nested {
            if let Some(ident) = extract_type_name(&vec_inner) {
                return IndexMapValueKind::VecNested(ident);
            }
        }
        if is_enum_tagged {
            if let Some(ident) = extract_type_name(&vec_inner) {
                return IndexMapValueKind::VecTaggedEnum(ident);
            }
        }
        // Otherwise it's a Vec of primitive
        return IndexMapValueKind::VecPrimitive;
    }

    // Not a Vec, check if it's a struct/enum type
    if is_nested {
        if let Some(ident) = extract_type_name(value_ty) {
            return IndexMapValueKind::Nested(ident);
        }
    }
    if is_enum_tagged {
        if let Some(ident) = extract_type_name(value_ty) {
            return IndexMapValueKind::TaggedEnum(ident);
        }
    }

    // Otherwise it's a primitive type
    IndexMapValueKind::Primitive
}

/// Arguments for the td_pmake_tagged_enum attribute
struct TaggedEnumArgs {
    tag_key: String,
    aliases: Vec<String>,
}

impl syn::parse::Parse for TaggedEnumArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // Parse: "tag_key" or "tag_key", aliases = ["alias1", "alias2"]
        let tag_key: syn::LitStr = input.parse()?;

        let mut aliases = Vec::new();

        // Check for optional aliases
        if input.peek(syn::Token![,]) {
            input.parse::<syn::Token![,]>()?;

            // Parse: aliases = [...]
            let ident: syn::Ident = input.parse()?;
            if ident != "aliases" {
                return Err(syn::Error::new(ident.span(), "expected 'aliases'"));
            }
            input.parse::<syn::Token![=]>()?;

            let content;
            syn::bracketed!(content in input);

            while !content.is_empty() {
                let alias: syn::LitStr = content.parse()?;
                aliases.push(alias.value());
                if content.peek(syn::Token![,]) {
                    content.parse::<syn::Token![,]>()?;
                }
            }
        }

        Ok(TaggedEnumArgs {
            tag_key: tag_key.value(),
            aliases,
        })
    }
}

/// Tag tagged enums for use with `deserialize`
///
/// # Example
///
/// ```rust,ignore
/// #[tpd_make_tagged_enum("kind", aliases = ["type"])]
/// #[derive(Debug)]
/// enum EitherOne {
///     KindA(InnerA),
///     KindB(InnerB),
/// }
/// ```
///
/// This generates:
/// - A `PartialEitherOne` enum with partial variants
/// - Implementation of `ToConcrete<EitherOne>` for `PartialEitherOne`
/// - Implementation of `FromTomlTable` for error collection
#[proc_macro_attribute]
pub fn tpd_make_tagged_enum(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as TaggedEnumArgs);
    let input = parse_macro_input!(item as DeriveInput);
    let enum_name = &input.ident;
    let partial_name = format_ident!("Partial{}", enum_name);
    let generics = &input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let tag_key = &args.tag_key;
    let tag_aliases = &args.aliases;

    let variants = match &input.data {
        Data::Enum(data) => &data.variants,
        _ => panic!("tpd_make_tagged_enum only supports enums"),
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
                    quote_spanned! { input.ident.span() =>
                        #variant_name(#partial_inner_type)
                    }
                }
                _ => panic!("tpd_make_tagged_enum only supports single unnamed field variants"),
            }
        })
        .collect();

    // Can concrete implementation
    let can_concrete_variants: Vec<_> = variants
        .iter()
        .map(|v| {
            let variant_name = &v.ident;
            quote_spanned! { input.ident.span() =>
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
            quote_spanned! { input.ident.span() =>
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
                    quote_spanned! { input.ident.span() =>
                        #variant_name_str => {
                            let result: ::toml_pretty_deser::TomlValue<#partial_inner_type> = ::toml_pretty_deser::deserialize_nested(
                                item,
                                &(0..0),
                                col,
                                fields_to_ignore,
                            );
                            result.value.map(#partial_name::#variant_name)
                        }
                    }
                }
                _ => unreachable!(),
            }
        })
        .collect();

    let expanded = quote_spanned! { input.ident.span() =>
        #input

        #[derive(Debug, Clone)]
        #generics
        enum #partial_name #ty_generics #where_clause {
            #(#partial_variants,)*
        }

        impl #impl_generics ::toml_pretty_deser::TaggedEnumMeta for #partial_name #ty_generics #where_clause {
            const TAG_KEY: &'static str = #tag_key;
            const TAG_ALIASES: &'static [&'static str] = &[#(#tag_aliases),*];

            fn all_variant_names() -> &'static [&'static str] {
                &[#(#variant_names),*]
            }

            fn deserialize_variant(
                variant_name: &str,
                item: &::toml_edit::Item,
                col: &TomlCollector,
                fields_to_ignore: &[&str]
            ) -> Option<Self> {
                match variant_name {
                    #(#deserialize_variant_arms,)*
                    _ => None,
                }
            }
        }

        impl #impl_generics FromTomlTable<#enum_name> for #partial_name #ty_generics #where_clause {
            fn can_concrete(&self) -> bool {
                match self {
                    #(#can_concrete_variants)*
                }
            }

            fn to_concrete(self) -> Option<#enum_name #ty_generics> {
                match self {
                    #(#to_concrete_variants,)*
                }
            }

            fn from_toml_table(_helper: &mut ::toml_pretty_deser::TomlHelper<'_>) -> Self {
                panic!("FromTomlTable should not be called directly on tagged enums. Use TaggedEnumMeta::deserialize_variant instead.");
            }
        }
        //this one happens if nested is set
        impl #impl_generics FromTomlItem for #partial_name #ty_generics #where_clause {
            fn from_toml_item(item: &toml_edit::Item, parent_span: std::ops::Range<usize>,
                col: &TomlCollector
        ) -> TomlValue<Self> {
                let variant_names = Self::all_variant_names();
                match item.as_table_like_plus() {
                Some(table) => {
                    let mut helper = TomlHelper::from_item(item, col);
                    let tag_key = #tag_key;
                    let tag_aliases = &[#(#tag_aliases),*];
                    let tag_result: TomlValue<String> =
                            helper.get_with_aliases(tag_key, tag_aliases, false); //we handle the
                            // missing case below explicitly
                    // Build the list of fields to ignore (canonical key + all aliases)
                    let mut fields_to_ignore: Vec<&str> = vec![tag_key];
                    fields_to_ignore.extend(tag_aliases.iter().copied());
                    let span: std::ops::Range<usize> = table.span().unwrap_or(0..0);
                    match &tag_result.state {
                        TomlValueState::Ok { .. } => {
                            // Successfully found the tag value
                            let tag_str = tag_result.value.as_ref().expect("No avlue on TomlValueState::Ok. Bug");
                            let mut matched_variant: Option<&str> = None;

                            for variant_name in variant_names {
                                if col.match_mode.matches(variant_name, tag_str) {
                                    matched_variant = Some(variant_name);
                                    break;
                                }
                            }

                            if let Some(variant_name) = matched_variant {
                                // Deserialize the specific variant
                                match Self::deserialize_variant(
                                    variant_name,
                                    item,
                                    col,
                                    &fields_to_ignore,
                                ) {
                                    Some(partial) => TomlValue {
                                        value: Some(partial),
                                        state: TomlValueState::Ok { span},
                                    },
                                    None => TomlValue {
                                        value: None,
                                        state: TomlValueState::ValidationFailed {
                                            span,
                                            message: "Failed to deserialize variant".to_string(),
                                            help: None,
                                        },
                                    },
                                }
                            } else {
                                TomlValue {
                                    value: None,
                                    state: TomlValueState::ValidationFailed {
                                        span,
                                        message: "Unknown enum variant".to_string(),
                                        help: Some(suggest_alternatives(tag_str, variant_names)),
                                    },
                                }
                            }
                        }
                        TomlValueState::MultiDefined { key: _, spans } => TomlValue {
                            value: None,
                            state: TomlValueState::MultiDefined {
                                key: tag_key.to_string(),
                                spans: spans.to_vec(),
                            },
                        },
                        TomlValueState::WrongType {
                            span: wrong_span,
                            expected: _,
                            found,
                        } => TomlValue {
                            value: None,
                            state: TomlValueState::ValidationFailed {
                                span: wrong_span.clone(),
                                message: format!("Wrong type: {}, expected string", found),
                                help: Some(suggest_alternatives("", variant_names)),
                            },
                        },
                        TomlValueState::Missing { .. } => TomlValue {
                            value: None,
                            state: TomlValueState::ValidationFailed {
                                span,
                                message: format!("Missing required tag field: {}", tag_key),
                                help: Some(suggest_alternatives("", variant_names)),
                            },
                        },
                        _ => TomlValue {
                            value: None,
                            state: tag_result.state.clone(),
                        },
                    }

                },
                None => {
                    if let toml_edit::Item::None = item {
                        TomlValue::new_empty_missing(parent_span)
                    } else {
                        TomlValue::new_wrong_type(item, parent_span, "table | inline_table")
                    }
                }
            }
            }
        }
        //and this one it isn't
         impl #impl_generics FromTomlItem for #enum_name #ty_generics #where_clause {
            fn from_toml_item(item: &toml_edit::Item, parent_span: std::ops::Range<usize>,
                col: &TomlCollector
        ) -> TomlValue<Self> {
                let variant_names = #partial_name::all_variant_names();
                match item.as_table_like_plus() {
                Some(table) => {
                    let mut helper = TomlHelper::from_item(item, col);
                    let tag_key = #tag_key;
                    let tag_aliases = &[#(#tag_aliases),*];
                    let tag_result: TomlValue<String> =
                            helper.get_with_aliases(tag_key, tag_aliases, true);
                    // Build the list of fields to ignore (canonical key + all aliases)
                    let mut fields_to_ignore: Vec<&str> = vec![tag_key];
                    fields_to_ignore.extend(tag_aliases.iter().copied());
                    let span: std::ops::Range<usize> = table.span().unwrap_or(0..0);
                    match &tag_result.state {
                        TomlValueState::Ok { .. } => {
                            // Successfully found the tag value
                            let tag_str = tag_result.value.as_ref().expect("No value on TomlValueState::Ok. Bug");
                            let mut matched_variant: Option<&str> = None;

                            for variant_name in variant_names {
                                if col.match_mode.matches(variant_name, tag_str) {
                                    matched_variant = Some(variant_name);
                                    break;
                                }
                            }

                            if let Some(variant_name) = matched_variant {
                                // Deserialize the specific variant
                                match #partial_name::deserialize_variant(
                                    variant_name,
                                    item,
                                    col,
                                    &fields_to_ignore,
                                ) {
                                    Some(partial) => {
                                        if partial.can_concrete() {
                                            TomlValue {
                                                value: Some(partial.to_concrete().expect("to_concrete failed but can_concrete passed. Bug")),
                                                state: TomlValueState::Ok { span},
                                            }
                                        } else {
                                            TomlValue {
                                                value: None,
                                                state: TomlValueState::ValidationFailed {
                                                    span,
                                                    message: "Failed to deserialize variant".to_string(),
                                                    help: None,
                                                }
                                            }
                                        }
                                    }
                                    None => TomlValue {
                                        value: None,
                                        state: TomlValueState::ValidationFailed {
                                            span,
                                            message: "Failed to deserialize variant".to_string(),
                                            help: None,
                                        },
                                    },
                                }
                            } else {
                                TomlValue {
                                    value: None,
                                    state: TomlValueState::ValidationFailed {
                                        span,
                                        message: "Unknown enum variant".to_string(),
                                        help: Some(suggest_alternatives(tag_str, variant_names)),
                                    },
                                }
                            }
                        }
                        TomlValueState::MultiDefined { key: _, spans } => TomlValue {
                            value: None,
                            state: TomlValueState::MultiDefined {
                                key: tag_key.to_string(),
                                spans: spans.to_vec(),
                            },
                        },
                        TomlValueState::WrongType {
                            span: wrong_span,
                            expected: _,
                            found,
                        } => TomlValue {
                            value: None,
                            state: TomlValueState::ValidationFailed {
                                span: wrong_span.clone(),
                                message: format!("Wrong type: {}, expected string", found),
                                help: Some(suggest_alternatives("", variant_names)),
                            },
                        },
                        TomlValueState::Missing { .. } => TomlValue {
                            value: None,
                            state: TomlValueState::ValidationFailed {
                                span,
                                message: format!("Missing required tag field: {}", tag_key),
                                help: Some(suggest_alternatives("", variant_names)),
                            },
                        },
                        _ => TomlValue {
                            value: None,
                            state: tag_result.state.clone(),
                        },
                    }

                },
                None => {
                    if let toml_edit::Item::None = item {
                        TomlValue::new_empty_missing(parent_span)
                    } else {
                        TomlValue::new_wrong_type(item, parent_span, "table | inline_table")
                    }
                }
            }
            }
        }

    };

    TokenStream::from(expanded)
}

/// Tag a struct for use with `deserialize`
#[proc_macro_attribute]
pub fn tpd_make_partial(attr: TokenStream, item: TokenStream) -> TokenStream {
    // Parse the boolean argument (default to true)
    let generate_verify = if attr.is_empty() {
        true
    } else {
        let attr_str = attr.to_string();
        match attr_str.as_str() {
            "true" => true,
            "false" => false,
            _ => panic!("tpd_make_partial expects 'true' or 'false', got: {}", attr_str),
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
            _ => panic!("tpd_make_partial only supports structs with named fields"),
        },
        _ => panic!("tpd_make_partial only supports structs"),
    };

    // Create a version of the input struct without attributes
    let mut cleaned_input = input.clone();
    if let Data::Struct(ref mut data) = cleaned_input.data {
        if let Fields::Named(ref mut fields) = data.fields {
            for field in fields.named.iter_mut() {
                field.attrs.retain(|attr| {
                    !attr.path().is_ident("nested")
                        && !attr.path().is_ident("enum_tagged")
                        && !attr.path().is_ident("tpd_alias")
                        && !attr.path().is_ident("tpd_default_in_verify")
                });
            }
        }
    }

    // Validate nested and enum_tagged fields first
    for f in fields.iter() {
        let ty = &f.ty;
        // Skip validation for IndexMap types - they're validated via analyze_indexmap_value_type
        let is_indexmap = is_indexmap_type(ty) || is_option_indexmap_type(ty);

        if is_nested_field(f) && !is_indexmap {
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
        if is_enum_tagged_field(f) && !is_indexmap {
            // enum_tagged can be applied to EnumType, Option<EnumType>, or Vec<EnumType>
            // The tag key is now provided by the enum's TaggedEnumMeta impl, not here
            if is_option_type(ty) {
                if extract_option_inner_type(ty).is_none() {
                    panic!(
                        "enum_tagged attribute on Option field requires a simple inner type name"
                    );
                }
            } else if is_vec_type(ty) {
                if extract_vec_inner_type(ty).is_none() {
                    panic!("enum_tagged attribute on Vec field requires a simple inner type name");
                }
            } else if extract_type_name(ty).is_none() {
                panic!("enum_tagged attribute requires a simple type name");
            }
        }
    }

    let partial_fields: Vec<_> = fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            let ty = &f.ty;

            // Check for IndexMap types first
            if is_indexmap_type(ty) || is_option_indexmap_type(ty) {
                let is_optional = is_option_indexmap_type(ty);
                let value_ty = if is_optional {
                    extract_option_indexmap_value_type(ty).expect("Can't fail")
                } else {
                    extract_indexmap_value_type(ty).expect("Can't fail")
                };
                let kind = analyze_indexmap_value_type(&value_ty, f);

                match kind {
                    IndexMapValueKind::Nested(inner_name) => {
                        let partial_type = format_ident!("Partial{}", inner_name);
                        if is_optional {
                            quote! { #name: TomlValue<Option<indexmap::IndexMap<String, #partial_type>>> }
                        } else {
                            quote! { #name: TomlValue<indexmap::IndexMap<String, #partial_type>> }
                        }
                    }
                    IndexMapValueKind::TaggedEnum(inner_name) => {
                        let partial_type = format_ident!("Partial{}", inner_name);
                        if is_optional {
                            quote! { #name: TomlValue<Option<indexmap::IndexMap<String, #partial_type>>> }
                        } else {
                            quote! { #name: TomlValue<indexmap::IndexMap<String, #partial_type>> }
                        }
                    }
                    IndexMapValueKind::VecNested(inner_name) => {
                        let partial_type = format_ident!("Partial{}", inner_name);
                        if is_optional {
                            quote! { #name: TomlValue<Option<indexmap::IndexMap<String, Vec<#partial_type>>>> }
                        } else {
                            quote! { #name: TomlValue<indexmap::IndexMap<String, Vec<#partial_type>>> }
                        }
                    }
                    IndexMapValueKind::VecTaggedEnum(inner_name) => {
                        let partial_type = format_ident!("Partial{}", inner_name);
                        if is_optional {
                            quote! { #name: TomlValue<Option<indexmap::IndexMap<String, Vec<#partial_type>>>> }
                        } else {
                            quote! { #name: TomlValue<indexmap::IndexMap<String, Vec<#partial_type>>> }
                        }
                    }
                    // For primitives and regular enums, keep the original type
                    _ => {
                        if is_optional {
                            // Extract the inner IndexMap type from Option<IndexMap<...>>
                            quote! { #name: TomlValue<#ty> }
                        } else {
                            quote! { #name: TomlValue<#ty> }
                        }
                    }
                }
            } else if is_nested_field(f) {
                // Check if this is Option<InnerType>, Vec<InnerType>, or just InnerType
                if is_option_type(ty) {
                    // For Option<Nested> fields, use Option<PartialType>
                    let inner_type_name = extract_option_inner_type(ty).expect("can't fail");
                    let partial_type = format_ident!("Partial{}", inner_type_name);
                    quote! {
                        #name: TomlValue<Option<#partial_type>>
                    }
                } else if is_vec_type(ty) {
                    // For Vec<Nested> fields, use Vec<PartialType>
                    let inner_type_name = extract_vec_inner_type(ty).expect("can't fail");
                    let partial_type = format_ident!("Partial{}", inner_type_name);
                    quote! {
                        #name: TomlValue<Vec<#partial_type>>
                    }
                } else {
                    // For regular nested fields, use Partial{Type}
                    let type_name = extract_type_name(ty).expect("can't fail");
                    let partial_type = format_ident!("Partial{}", type_name);
                    quote! {
                        #name: TomlValue<#partial_type>
                    }
                }
            } else if is_enum_tagged_field(f) {
                // For enum_tagged fields, handle Option<EnumType>, Vec<EnumType>, or just EnumType
                if is_option_type(ty) {
                    let inner_type_name = extract_option_inner_type(ty).expect("can't fail");
                    let partial_type = format_ident!("Partial{}", inner_type_name);
                    quote! {
                        #name: TomlValue<Option<#partial_type>>
                    }
                } else if is_vec_type(ty) {
                    let inner_type_name = extract_vec_inner_type(ty).expect("can't fail");
                    let partial_type = format_ident!("Partial{}", inner_type_name);
                    quote! {
                        #name: TomlValue<Vec<#partial_type>>
                    }
                } else {
                    let type_name = extract_type_name(ty).expect("can't fail");
                    let partial_type = format_ident!("Partial{}", type_name);
                    quote! {
                        #name: TomlValue<#partial_type>
                    }
                }
            } else {
                quote! {
                    #name: TomlValue<#ty>
                }
            }
        })
        .collect();

    let can_concrete_fields: Vec<_> = fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            let ty = &f.ty;

            // Check for IndexMap types first
            if is_indexmap_type(ty) || is_option_indexmap_type(ty) {
                let is_optional = is_option_indexmap_type(ty);
                let value_ty = if is_optional {
                    extract_option_indexmap_value_type(ty).expect("can't fail")
                } else {
                    extract_indexmap_value_type(ty).expect("can't fail")
                };
                let kind = analyze_indexmap_value_type(&value_ty, f);

                match kind {
                    IndexMapValueKind::Nested(_) | IndexMapValueKind::TaggedEnum(_) => {
                        if is_optional {
                            quote! {
                                self.#name.value.as_ref().map(|opt| {
                                    opt.as_ref().map(|map| map.values().all(|p| p.can_concrete())).unwrap_or(true)
                                }).unwrap_or(false)
                            }
                        } else {
                            quote! {
                                self.#name.value.as_ref().map(|map| {
                                    map.values().all(|p| p.can_concrete())
                                }).unwrap_or(false)
                            }
                        }
                    }
                    IndexMapValueKind::VecNested(_) | IndexMapValueKind::VecTaggedEnum(_) => {
                        if is_optional {
                            quote! {
                                self.#name.value.as_ref().map(|opt| {
                                    opt.as_ref().map(|map| map.values().all(|vec| vec.iter().all(|p| p.can_concrete()))).unwrap_or(true)
                                }).unwrap_or(false)
                            }
                        } else {
                            quote! {
                                self.#name.value.as_ref().map(|map| {
                                    map.values().all(|vec| vec.iter().all(|p| p.can_concrete()))
                                }).unwrap_or(false)
                            }
                        }
                    }
                    // For primitives and regular enums, just check has_value
                    _ => {
                        quote! {
                            self.#name.has_value()
                        }
                    }
                }
            } else if is_nested_field(f) {
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
                if is_option_type(&f.ty) {
                    // For Option<EnumTagged>, it's concrete if Some and inner can_concrete, or if None
                    quote! {
                        self.#name.value.as_ref().map(|opt| {
                            opt.as_ref().map(|p| p.can_concrete()).unwrap_or(true)
                        }).unwrap_or(false)
                    }
                } else if is_vec_type(&f.ty) {
                    // For Vec<EnumTagged>, all items must be concrete
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
            let ty = &f.ty;

            // Check for IndexMap types first
            if is_indexmap_type(ty) || is_option_indexmap_type(ty) {
                let is_optional = is_option_indexmap_type(ty);
                let value_ty = if is_optional {
                    extract_option_indexmap_value_type(ty).unwrap()
                } else {
                    extract_indexmap_value_type(ty).unwrap()
                };
                let kind = analyze_indexmap_value_type(&value_ty, f);

                match kind {
                    IndexMapValueKind::Nested(_) | IndexMapValueKind::TaggedEnum(_) => {
                        if is_optional {
                            quote! {
                                #name: self.#name.value.flatten().map(|map| {
                                    map.into_iter().filter_map(|(k, p)| p.to_concrete().map(|v| (k, v))).collect()
                                })
                            }
                        } else {
                            quote! {
                                #name: self.#name.value.map(|map| {
                                    map.into_iter().filter_map(|(k, p)| p.to_concrete().map(|v| (k, v))).collect()
                                }).expect("was checked by can_concrete before")
                            }
                        }
                    }
                    IndexMapValueKind::VecNested(_) | IndexMapValueKind::VecTaggedEnum(_) => {
                        if is_optional {
                            quote! {
                                #name: self.#name.value.flatten().map(|map| {
                                    map.into_iter().map(|(k, vec)| {
                                        (k, vec.into_iter().filter_map(|p| p.to_concrete()).collect())
                                    }).collect()
                                })
                            }
                        } else {
                            quote! {
                                #name: self.#name.value.map(|map| {
                                    map.into_iter().map(|(k, vec)| {
                                        (k, vec.into_iter().filter_map(|p| p.to_concrete()).collect())
                                    }).collect()
                                }).expect("was checked by can_concrete before")
                            }
                        }
                    }
                    // For primitives and regular enums, just unwrap
                    _ => {
                        quote! {
                            #name: self.#name.expect("was checked by can_concrete before")
                        }
                    }
                }
            } else if is_nested_field(f) {
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
                        }).expect("was checked by can_concrete before")
                    }
                } else {
                    // For regular nested fields, convert Partial to Concrete
                    quote! {
                        #name: self.#name.value.and_then(|p| p.to_concrete()).expect("was checked by can_concrete before")
                    }
                }
            } else if is_enum_tagged_field(f) {
                // For enum_tagged fields, convert PartialEnum to ConcreteEnum
                if is_option_type(&f.ty) {
                    // For Option<EnumTagged>, convert Option<PartialEnum> to Option<ConcreteEnum>
                    // self.#name.value is Option<Option<PartialEnumType>>
                    quote! {
                        #name: self.#name.value.flatten().and_then(|p| p.to_concrete())
                    }
                } else if is_vec_type(&f.ty) {
                    // For Vec<EnumTagged>, convert each partial in Vec<PartialEnum> to Vec<ConcreteEnum>
                    quote! {
                        #name: self.#name.value.map(|vec| {
                            vec.into_iter().filter_map(|p| p.to_concrete()).collect()
                        }).unwrap_or_default()
                    }
                } else {
                    quote! {
                        #name: self.#name.value.and_then(|p| p.to_concrete()).expect("was checked by can_concrete before")
                    }
                }
            } else {
                quote! {
                    #name: self.#name.expect("was checked by can_concrete before")
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
            let ty = &f.ty;

            // Check for IndexMap types first
            if is_indexmap_type(ty) || is_option_indexmap_type(ty) {
                let is_optional = is_option_indexmap_type(ty);

                if is_optional {
                    quote! {
                        #name: toml_item_as_map(
                                helper.get_with_aliases::<::toml_edit::Item>(#name_str, &[#(#aliases),*], false),
                                &helper.col).into_optional()
                    }
                } else {
                    quote! {
                        #name: toml_item_as_map(
                                helper.get_with_aliases::<::toml_edit::Item>(#name_str, &[#(#aliases),*], true),
                                &helper.col)
                    }
                }
            }  else {
                // For regular fields, use aliases if present
                if is_option_type(ty) {
                    quote! {
                        #name: {
                            let t: TomlValue<_> = helper.get_with_aliases(#name_str, &[#(#aliases),*], false);
                            //do we really need this? Replace value with None for a test.
                            if let TomlValueState::Missing {parent_span, ..} = t.state {
                                TomlValue{
                                    value: Some(None),
                                    state: TomlValueState::Ok{span: parent_span},
                                }
                            } else {
                                t
                            }
                        }
                    }
                } else  {
                    let missing_is_error = !is_defaulted_field(f);
                    quote! {
                        #name: helper.get_with_aliases(#name_str, &[#(#aliases),*], #missing_is_error)
                    }
                }
            }
        })
        .collect();

    // Conditionally generate VerifyFromToml impl
    let generate_verify_impl = if generate_verify {
        quote! {
            impl #impl_generics VerifyFromToml for #partial_name #ty_generics #where_clause {
                fn verify(self, _helper: &mut TomlHelper<'_>) -> Self {
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

        // impl #impl_generics PartialEq for #partial_name #ty_generics #where_clause {
        //     fn eq(&self, other: &Self) -> bool {
        //         true
        //     }
        // }

        impl #impl_generics FromTomlTable<#struct_name> for #partial_name #ty_generics #where_clause {
            fn can_concrete(&self) -> bool {
                #(#can_concrete_fields)&&*
            }

            fn to_concrete(self) -> Option<#struct_name #ty_generics> {
                Some(#struct_name {
                    #(#to_concrete_fields,)*
                })
            }

            fn from_toml_table(helper: &mut TomlHelper<'_>) -> Self {
                #partial_name {
                    #(#from_toml_table_fields,)*
                }
            }
        }
        impl #impl_generics FromTomlItem for #partial_name #ty_generics #where_clause {
            fn from_toml_item(item: &toml_edit::Item, parent_span: std::ops::Range<usize>,
                col: &TomlCollector
            ) -> TomlValue<Self> {
                match item.as_table_like_plus() {
                    Some(table) => {
                        let mut helper = TomlHelper::from_table(table,
                            col.clone());
                        let partial = #partial_name::from_toml_table(&mut helper).verify(&mut helper);
                        helper.deny_unknown();

                        if partial.can_concrete()  {
                            TomlValue {
                                    value: Some(partial),
                                    state: TomlValueState::Ok {
                                        span: table.span().unwrap_or(parent_span).clone(),
                                    }
                            }
                        } else {
                            TomlValue {
                                    value: Some(partial),
                                    state: TomlValueState::Nested {
                                    },
                            }
                        }
                    },
                    None => {
                            match item {
                                toml_edit::Item::None => TomlValue::new_empty_missing(parent_span.clone()),
                                _=> TomlValue::new_wrong_type(item, parent_span, "Table-like for Partial struct")
                            }

                    }
                }
            }
        }

        #generate_verify_impl
    };

    TokenStream::from(expanded)
}
