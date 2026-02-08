use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{
    Data, DeriveInput, Fields, GenericArgument, PathArguments, Type, TypePath, parse_macro_input,
};

/// Unified macro for TOML pretty deserialization
///
/// Automatically detects the type and generates appropriate code:
///
/// - **Structs with named fields**: Generates a `Partial{Struct}` struct with deserialization support
/// - **Tuple structs (newtype)**: Implements `FromTomlItem` that delegates to the inner type
/// - **Unit enums**: Implements `StringNamedEnum` and `FromTomlItem` for enum variants
/// - **Tagged enums**: Generates `Partial{Enum}` with `TaggedEnumMeta` implementation
///
/// # Examples
///
/// ## Structs
///
/// ```ignore
/// #[tpd]                                          // With VerifyFromToml impl
/// #[tpd(partial = false)]                         // Without VerifyFromToml impl
/// #[derive(Debug)]
/// struct Config {
///     name: String,
///     count: u32,
///     #[tpd_nested]
///     nested: InnerStruct,
/// }
/// ```
///
/// ## Tuple Structs (Newtype)
///
/// ```ignore
/// #[tpd]
/// #[derive(Debug, PartialEq, Eq)]
/// struct MyUnit(String);
/// ```
///
/// ## Unit Enums
///
/// ```ignore
/// #[tpd]
/// #[derive(Debug, Clone)]
/// enum Color {
///     Red,
///     Green,
///     Blue,
/// }
/// ```
///
/// ## Tagged Enums
///
/// ```ignore
/// #[tpd(tag = "kind")]                            // Basic tagged enum
/// #[tpd(tag = "kind", aliases = ["type"])]        // With tag aliases
/// #[derive(Debug)]
/// enum EitherOne {
///     KindA(InnerA),
///     KindB(InnerB),
/// }
/// ```
#[proc_macro_attribute]
pub fn tpd(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = if attr.is_empty() {
        TpdArgs::default()
    } else {
        parse_macro_input!(attr as TpdArgs)
    };

    let input = parse_macro_input!(item as DeriveInput);

    match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(_) => {
                if args.tag.is_some() {
                    panic!("tag argument is only valid for tagged enums");
                }
                handlers::handle_struct(input, args.partial.unwrap_or(true))
            }
            Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                if args.tag.is_some() {
                    panic!("tag argument is only valid for tagged enums");
                }
                handlers::handle_tuple_struct(input)
            }
            _ => panic!(
                "#[tpd] only supports structs with named fields or single-field tuple structs"
            ),
        },
        Data::Enum(data) => {
            let has_tag = args.tag.is_some();
            let all_unit = data
                .variants
                .iter()
                .all(|v| matches!(v.fields, Fields::Unit));
            let all_single_unnamed = data
                .variants
                .iter()
                .all(|v| matches!(&v.fields, Fields::Unnamed(f) if f.unnamed.len() == 1));

            match (has_tag, all_unit, all_single_unnamed) {
                (true, false, true) => handlers::handle_tagged_enum(
                    input,
                    args.tag.expect("tag required for tagged enums"),
                    args.aliases,
                ),
                (false, true, _) => handlers::handle_unit_enum(input),
                (false, false, true) => {
                    panic!(
                        "For tagged enums, you must specify tag = \"key\". Example: #[tpd(tag = \"kind\")]"
                    );
                }
                _ => panic!(
                    "#[tpd] only supports:\n\
                     - Structs with named fields\n\
                     - Unit enums (all variants without fields)\n\
                     - Tagged enums (all variants with single unnamed field) - requires tag argument\n\n\
                     Your enum has mixed or unsupported field types."
                ),
            }
        }
        Data::Union(_) => panic!("#[tpd] does not support unions"),
    }
}

/// Arguments for the `#[tpd]` attribute
#[derive(Default)]
struct TpdArgs {
    partial: Option<bool>,
    tag: Option<String>,
    aliases: Vec<String>,
}

impl syn::parse::Parse for TpdArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut args = TpdArgs::default();

        while !input.is_empty() {
            let ident: syn::Ident = input.parse()?;

            match ident.to_string().as_str() {
                "partial" => {
                    input.parse::<syn::Token![=]>()?;
                    let value: syn::LitBool = input.parse()?;
                    args.partial = Some(value.value());
                }
                "tag" => {
                    input.parse::<syn::Token![=]>()?;
                    let value: syn::LitStr = input.parse()?;
                    args.tag = Some(value.value());
                }
                "aliases" => {
                    input.parse::<syn::Token![=]>()?;
                    let content;
                    syn::bracketed!(content in input);
                    while !content.is_empty() {
                        let alias: syn::LitStr = content.parse()?;
                        args.aliases.push(alias.value());
                        if content.peek(syn::Token![,]) {
                            content.parse::<syn::Token![,]>()?;
                        }
                    }
                }
                _ => {
                    return Err(syn::Error::new(
                        ident.span(),
                        format!(
                            "unknown argument `{}`, expected: partial, tag, or aliases",
                            ident
                        ),
                    ));
                }
            }

            if !input.is_empty() {
                input.parse::<syn::Token![,]>()?;
            }
        }

        Ok(args)
    }
}

/// Clean attributes from struct fields, removing our custom attributes
fn clean_struct_input(input: &mut DeriveInput) {
    if let Data::Struct(ref mut data) = input.data
        && let Fields::Named(ref mut fields) = data.fields
    {
        for field in &mut fields.named {
            field.attrs.retain(|attr| {
                !attr.path().is_ident("tpd_nested")
                    && !attr.path().is_ident("tpd_alias")
                    && !attr.path().is_ident("tpd_default_in_verify")
                    && !attr.path().is_ident("tpd_default")
                    && !attr.path().is_ident("tpd_skip")
                    && !attr.path().is_ident("tpd_adapt_in_verify")
                    && !attr.path().is_ident("tpd_with")
                    && !attr.path().is_ident("tpd_absorb_remaining")
            });
        }
    }
}

/// Clean attributes from enum variants, removing our custom attributes
fn clean_enum_input(input: &mut DeriveInput) {
    if let Data::Enum(ref mut data) = input.data {
        for variant in &mut data.variants {
            variant.attrs.retain(|attr| !attr.path().is_ident("tpd_alias"));
        }
    }
}

/// Module for type analysis utilities
mod type_analysis {
    use super::*;

    pub fn is_nested_field(field: &syn::Field) -> bool {
        field
            .attrs
            .iter()
            .any(|attr| attr.path().is_ident("tpd_nested"))
    }

    pub fn is_defaulted_in_verify_field(field: &syn::Field) -> bool {
        field
            .attrs
            .iter()
            .any(|attr| attr.path().is_ident("tpd_default_in_verify"))
    }

    pub fn is_defaulted_field(field: &syn::Field) -> bool {
        field
            .attrs
            .iter()
            .any(|attr| attr.path().is_ident("tpd_default"))
    }

    pub fn is_skipped_field(field: &syn::Field) -> bool {
        field
            .attrs
            .iter()
            .any(|attr| attr.path().is_ident("tpd_skip"))
    }

    /// Check if the skipped field should be available in verify().
    /// Returns true if the attribute is `#[tpd_skip(true)]` or `#[tpd_skip]` (default is true).
    /// Returns false if the attribute is `#[tpd_skip(false)]`.
    pub fn is_skipped_field_available_in_verify(field: &syn::Field) -> bool {
        for attr in &field.attrs {
            if attr.path().is_ident("tpd_skip") {
                // Try to parse the argument - if no argument or parse fails, default to true
                if let Ok(lit) = attr.parse_args::<syn::LitBool>() {
                    return lit.value();
                }
                // No argument means default behavior (true - available in verify)
                return true;
            }
        }
        false
    }

    pub fn is_absorb_remaining_field(field: &syn::Field) -> bool {
        field
            .attrs
            .iter()
            .any(|attr| attr.path().is_ident("tpd_absorb_remaining"))
    }

    pub fn is_adapt_in_verify_field(field: &syn::Field) -> bool {
        field
            .attrs
            .iter()
            .any(|attr| attr.path().is_ident("tpd_adapt_in_verify"))
    }

    /// Extract the function path from `#[tpd_with(function)]` attribute
    /// Returns the TokenStream of the function path (e.g., `my_func` or `module::my_func`)
    pub fn extract_tpd_with_function(field: &syn::Field) -> Option<proc_macro2::TokenStream> {
        for attr in &field.attrs {
            if attr.path().is_ident("tpd_with") {
                let result: Result<syn::Path, _> = attr.parse_args();
                match result {
                    Ok(path) => return Some(quote! { #path }),
                    Err(e) => panic!("Failed to parse tpd_with function path: {}", e),
                }
            }
        }
        None
    }

    pub fn extract_aliases(field: &syn::Field) -> Vec<String> {
        let mut aliases = Vec::new();

        for attr in &field.attrs {
            if attr.path().is_ident("tpd_alias") {
                let result: Result<Vec<String>, _> =
                    attr.parse_args_with(|input: syn::parse::ParseStream| {
                        let mut names = Vec::new();
                        loop {
                            if input.is_empty() {
                                break;
                            }

                            if input.peek(syn::LitStr) {
                                let lit: syn::LitStr = input.parse()?;
                                names.push(lit.value());
                            } else {
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

    /// Extract aliases from an enum variant's #[tpd_alias(...)] attribute
    pub fn extract_variant_aliases(variant: &syn::Variant) -> Vec<String> {
        let mut aliases = Vec::new();

        for attr in &variant.attrs {
            if attr.path().is_ident("tpd_alias") {
                let result: Result<Vec<String>, _> =
                    attr.parse_args_with(|input: syn::parse::ParseStream| {
                        let mut names = Vec::new();
                        loop {
                            if input.is_empty() {
                                break;
                            }

                            if input.peek(syn::LitStr) {
                                let lit: syn::LitStr = input.parse()?;
                                names.push(lit.value());
                            } else {
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

    /// Extract the final type name from a type path.
    /// This handles both simple types like `InnerA` and qualified paths like `module::InnerA`.
    /// Returns the last segment's identifier (e.g., `InnerA` for both cases above).
    pub fn extract_type_name(ty: &Type) -> Option<syn::Ident> {
        match ty {
            Type::Path(TypePath { path, .. }) => {
                // Use last segment to handle both simple `Foo` and qualified `module::Foo` paths
                path.segments.last().map(|seg| seg.ident.clone())
            }
            _ => None,
        }
    }

    /// Generate the partial type path for a given type.
    /// For simple types like `InnerA`, returns `PartialInnerA`.
    /// For qualified paths like `module::InnerA`, returns `module::PartialInnerA`.
    pub fn make_partial_type_path(ty: &Type) -> Option<proc_macro2::TokenStream> {
        match ty {
            Type::Path(TypePath { qself, path }) => {
                if qself.is_some() {
                    // Don't support qualified self types like <T as Trait>::Type
                    return None;
                }
                let segments = &path.segments;
                if segments.is_empty() {
                    return None;
                }

                // Get the last segment and create a Partial version
                let last_seg = segments.last()?;
                let partial_ident = format_ident!("Partial{}", last_seg.ident);

                // If there's only one segment, just return the partial ident
                if segments.len() == 1 {
                    return Some(quote! { #partial_ident });
                }

                // Otherwise, build the full path with module prefix
                // Take all segments except the last one as the prefix
                let prefix_segments: Vec<_> = segments.iter().take(segments.len() - 1).collect();
                Some(quote! { #(#prefix_segments::)* #partial_ident })
            }
            _ => None,
        }
    }

    /// Extract the inner type from Option<T> and return a TokenStream for Partial{T}
    /// Handles both `Option<Foo>` and `Option<module::Foo>`.
    pub fn make_partial_option_inner_type(ty: &Type) -> Option<proc_macro2::TokenStream> {
        match ty {
            Type::Path(TypePath { path, .. }) => {
                if let Some(segment) = path.segments.last()
                    && segment.ident == "Option"
                    && let PathArguments::AngleBracketed(args) = &segment.arguments
                    && let Some(GenericArgument::Type(inner_ty)) = args.args.first()
                {
                    return make_partial_type_path(&inner_ty);
                }
                None
            }
            _ => None,
        }
    }

    /// Extract the inner type from Vec<T> and return a TokenStream for Partial{T}
    /// Handles both `Vec<Foo>` and `Vec<module::Foo>`.
    pub fn make_partial_vec_inner_type(ty: &Type) -> Option<proc_macro2::TokenStream> {
        match ty {
            Type::Path(TypePath { path, .. }) => {
                if let Some(segment) = path.segments.last()
                    && segment.ident == "Vec"
                    && let PathArguments::AngleBracketed(args) = &segment.arguments
                    && let Some(GenericArgument::Type(inner_ty)) = args.args.first()
                {
                    return make_partial_type_path(&inner_ty);
                }
                None
            }
            _ => None,
        }
    }

    /// Extract the inner type from Option<Vec<T>> and return a TokenStream for Partial{T}
    /// Handles both `Option<Vec<Foo>>` and `Option<Vec<module::Foo>>`.
    pub fn make_partial_option_vec_inner_type(ty: &Type) -> Option<proc_macro2::TokenStream> {
        match ty {
            Type::Path(TypePath { path, .. }) => {
                if let Some(segment) = path.segments.last()
                    && segment.ident == "Option"
                    && let PathArguments::AngleBracketed(args) = &segment.arguments
                    && let Some(GenericArgument::Type(inner_ty)) = args.args.first()
                {
                    return make_partial_vec_inner_type(&inner_ty);
                }
                None
            }
            _ => None,
        }
    }

    /// Extract the inner type from Box<T> and return a TokenStream for Partial{T}
    /// Handles both `Box<Foo>` and `Box<module::Foo>`.
    pub fn make_partial_box_inner_type(ty: &Type) -> Option<proc_macro2::TokenStream> {
        match ty {
            Type::Path(TypePath { path, .. }) => {
                if let Some(segment) = path.segments.last()
                    && segment.ident == "Box"
                    && let PathArguments::AngleBracketed(args) = &segment.arguments
                    && let Some(GenericArgument::Type(inner_ty)) = args.args.first()
                {
                    return make_partial_type_path(&inner_ty);
                }
                None
            }
            _ => None,
        }
    }

    pub fn is_option_type(ty: &Type) -> bool {
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

    pub fn extract_option_inner_type(ty: &Type) -> Option<syn::Ident> {
        match ty {
            Type::Path(TypePath { path, .. }) => {
                if let Some(segment) = path.segments.last()
                    && segment.ident == "Option"
                    && let PathArguments::AngleBracketed(args) = &segment.arguments
                    && let Some(GenericArgument::Type(inner_ty)) = args.args.first()
                {
                    return extract_type_name(&inner_ty);
                }
                None
            }
            _ => None,
        }
    }

    pub fn is_vec_type(ty: &Type) -> bool {
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

    pub fn is_box_type(ty: &Type) -> bool {
        match ty {
            Type::Path(TypePath { path, .. }) => {
                if let Some(segment) = path.segments.last() {
                    segment.ident == "Box"
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn extract_vec_inner_type(ty: &Type) -> Option<syn::Ident> {
        match ty {
            Type::Path(TypePath { path, .. }) => {
                if let Some(segment) = path.segments.last()
                    && segment.ident == "Vec"
                    && let PathArguments::AngleBracketed(args) = &segment.arguments
                    && let Some(GenericArgument::Type(inner_ty)) = args.args.first()
                {
                    return extract_type_name(&inner_ty);
                }
                None
            }
            _ => None,
        }
    }

    pub fn is_indexmap_type(ty: &Type) -> bool {
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

    pub fn is_option_indexmap_type(ty: &Type) -> bool {
        match ty {
            Type::Path(TypePath { path, .. }) => {
                if let Some(segment) = path.segments.last()
                    && segment.ident == "Option"
                    && let PathArguments::AngleBracketed(args) = &segment.arguments
                    && let Some(GenericArgument::Type(inner_ty)) = args.args.first()
                {
                    return is_indexmap_type(&inner_ty);
                }
                false
            }
            _ => false,
        }
    }

    pub fn extract_indexmap_value_type(ty: &Type) -> Option<Type> {
        match ty {
            Type::Path(TypePath { path, .. }) => {
                if let Some(segment) = path.segments.last()
                    && segment.ident == "IndexMap"
                    && let PathArguments::AngleBracketed(args) = &segment.arguments
                {
                    let mut iter = args.args.iter();
                    iter.next();
                    if let Some(GenericArgument::Type(value_ty)) = iter.next() {
                        return Some(value_ty.clone());
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Extract the key type (first generic parameter) from IndexMap<K, V>
    pub fn extract_indexmap_key_type(ty: &Type) -> Option<Type> {
        match ty {
            Type::Path(TypePath { path, .. }) => {
                if let Some(segment) = path.segments.last()
                    && segment.ident == "IndexMap"
                    && let PathArguments::AngleBracketed(args) = &segment.arguments
                    && let Some(GenericArgument::Type(key_ty)) = args.args.first()
                {
                    return Some(key_ty.clone());
                }
                None
            }
            _ => None,
        }
    }

    /// Extract the key type from Option<IndexMap<K, V>>
    pub fn extract_option_indexmap_key_type(ty: &Type) -> Option<Type> {
        match ty {
            Type::Path(TypePath { path, .. }) => {
                if let Some(segment) = path.segments.last()
                    && segment.ident == "Option"
                    && let PathArguments::AngleBracketed(args) = &segment.arguments
                    && let Some(GenericArgument::Type(inner_ty)) = args.args.first()
                {
                    return extract_indexmap_key_type(&inner_ty);
                }
                None
            }
            _ => None,
        }
    }

    pub fn extract_option_indexmap_value_type(ty: &Type) -> Option<Type> {
        match ty {
            Type::Path(TypePath { path, .. }) => {
                if let Some(segment) = path.segments.last()
                    && segment.ident == "Option"
                    && let PathArguments::AngleBracketed(args) = &segment.arguments
                    && let Some(GenericArgument::Type(inner_ty)) = args.args.first()
                {
                    return extract_indexmap_value_type(&inner_ty);
                }
                None
            }
            _ => None,
        }
    }

    pub fn extract_vec_inner_full_type(ty: &Type) -> Option<Type> {
        match ty {
            Type::Path(TypePath { path, .. }) => {
                if let Some(segment) = path.segments.last()
                    && segment.ident == "Vec"
                    && let PathArguments::AngleBracketed(args) = &segment.arguments
                    && let Some(GenericArgument::Type(inner_ty)) = args.args.first()
                {
                    return Some(inner_ty.clone());
                }
                None
            }
            _ => None,
        }
    }

    #[derive(Clone)]
    pub enum IndexMapValueKind {
        Primitive,
        Nested(proc_macro2::TokenStream),
        VecNested(proc_macro2::TokenStream),
    }

    pub fn analyze_indexmap_value_type(value_ty: &Type, field: &syn::Field) -> IndexMapValueKind {
        let is_nested = is_nested_field(field);

        if let Some(vec_inner) = extract_vec_inner_full_type(value_ty) {
            if is_nested {
                if let Some(partial_type) = make_partial_type_path(&vec_inner) {
                    return IndexMapValueKind::VecNested(partial_type);
                }
            }
        }

        if is_nested {
            if let Some(partial_type) = make_partial_type_path(value_ty) {
                return IndexMapValueKind::Nested(partial_type);
            }
        }

        IndexMapValueKind::Primitive
    }

    pub fn is_unit_enum_variant(variant: &syn::Variant) -> bool {
        matches!(variant.fields, Fields::Unit)
    }

    pub fn is_single_unnamed_field_variant(variant: &syn::Variant) -> bool {
        matches!(&variant.fields, Fields::Unnamed(f) if f.unnamed.len() == 1)
    }

    /// Check if a type is Option<Vec<T>>
    pub fn is_option_vec_type(ty: &Type) -> bool {
        match ty {
            Type::Path(TypePath { path, .. }) => {
                if let Some(segment) = path.segments.last()
                    && segment.ident == "Option"
                    && let PathArguments::AngleBracketed(args) = &segment.arguments
                    && let Some(GenericArgument::Type(inner_ty)) = args.args.first()
                {
                    return is_vec_type(&inner_ty);
                }
                false
            }
            _ => false,
        }
    }

    /// Extract the inner type from Option<Vec<T>>, returning T's identifier
    pub fn extract_option_vec_inner_type(ty: &Type) -> Option<syn::Ident> {
        match ty {
            Type::Path(TypePath { path, .. }) => {
                if let Some(segment) = path.segments.last()
                    && segment.ident == "Option"
                    && let PathArguments::AngleBracketed(args) = &segment.arguments
                    && let Some(GenericArgument::Type(inner_ty)) = args.args.first()
                {
                    return extract_vec_inner_type(&inner_ty);
                }
                None
            }
            _ => None,
        }
    }

    /// Check if a type is Option<Box<T>>
    pub fn is_option_box_type(ty: &Type) -> bool {
        match ty {
            Type::Path(TypePath { path, .. }) => {
                if let Some(segment) = path.segments.last()
                    && segment.ident == "Option"
                    && let PathArguments::AngleBracketed(args) = &segment.arguments
                    && let Some(GenericArgument::Type(inner_ty)) = args.args.first()
                {
                    return is_box_type(&inner_ty);
                }
                false
            }
            _ => false,
        }
    }

    /// Extract the inner type from Option<Box<T>> and return a TokenStream for Partial{T}
    /// Handles both `Option<Box<Foo>>` and `Option<Box<module::Foo>>`.
    pub fn make_partial_option_box_inner_type(ty: &Type) -> Option<proc_macro2::TokenStream> {
        match ty {
            Type::Path(TypePath { path, .. }) => {
                if let Some(segment) = path.segments.last()
                    && segment.ident == "Option"
                    && let PathArguments::AngleBracketed(args) = &segment.arguments
                    && let Some(GenericArgument::Type(inner_ty)) = args.args.first()
                {
                    return make_partial_box_inner_type(&inner_ty);
                }
                None
            }
            _ => None,
        }
    }
}

/// Module for code generation functions
mod codegen {
    use super::*;
    use crate::type_analysis::*;
    use proc_macro2::TokenStream as TokenStream2;
    use syn::spanned::Spanned;

    pub fn gen_string_named_enum_impl(
        enum_name: &syn::Ident,
        impl_generics: &syn::ImplGenerics,
        ty_generics: &syn::TypeGenerics,
        where_clause: Option<&syn::WhereClause>,
        variants: &[syn::Variant],
    ) -> TokenStream2 {
        let variant_names: Vec<_> = variants.iter().map(|v| v.ident.to_string()).collect();

        // Collect all names including aliases for suggestion purposes
        let all_names_with_aliases: Vec<_> = variants
            .iter()
            .flat_map(|v| {
                let primary = v.ident.to_string();
                let aliases = extract_variant_aliases(v);
                std::iter::once(primary).chain(aliases)
            })
            .collect();

        // Generate match arms for each variant, including aliases
        let from_str_arms = variants.iter().map(|v| {
            let name = &v.ident;
            let name_str = name.to_string();
            let aliases = extract_variant_aliases(v);

            if aliases.is_empty() {
                quote! {
                    #name_str => ::std::option::Option::Some(#enum_name::#name)
                }
            } else {
                quote! {
                    #name_str #(| #aliases)* => ::std::option::Option::Some(#enum_name::#name)
                }
            }
        });

        quote! {
            impl #impl_generics ::toml_pretty_deser::StringNamedEnum for #enum_name #ty_generics #where_clause {
                fn all_variant_names() -> &'static [&'static str] {
                    &[#(#variant_names),*]
                }

                fn all_variant_names_with_aliases() -> &'static [&'static str] {
                    &[#(#all_names_with_aliases),*]
                }

                fn from_str(s: &str) -> ::std::option::Option<Self> {
                    match s {
                        #(#from_str_arms,)*
                        _ => ::std::option::Option::None,
                    }
                }
            }
        }
    }

    pub fn gen_from_toml_item_for_unit_enum(
        enum_name: &syn::Ident,
        impl_generics: &syn::ImplGenerics,
        ty_generics: &syn::TypeGenerics,
        where_clause: Option<&syn::WhereClause>,
        variants: &[syn::Variant],
    ) -> TokenStream2 {
        // Generate match arms for each variant, respecting match_mode
        // Each arm checks if the input string matches the variant name or any alias
        let match_arms = variants.iter().map(|v| {
            let name = &v.ident;
            let name_str = name.to_string();
            let aliases = extract_variant_aliases(v);

            if aliases.is_empty() {
                quote! {
                    if col.match_mode.matches(#name_str, s) {
                        return ::toml_pretty_deser::TomlValue::new_ok(#enum_name::#name, span);
                    }
                }
            } else {
                quote! {
                    if col.match_mode.matches(#name_str, s) #(|| col.match_mode.matches(#aliases, s))* {
                        return ::toml_pretty_deser::TomlValue::new_ok(#enum_name::#name, span);
                    }
                }
            }
        });

        quote! {
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

                            // Check each variant using match_mode for case-insensitive matching
                            #(#match_arms)*

                            // No match found
                            let help = ::toml_pretty_deser::suggest_enum_alternatives::<#enum_name>(s);
                            let res = ::toml_pretty_deser::TomlValue::new_validation_failed(
                                span,
                                "Invalid enum variant.".to_string(),
                                ::std::option::Option::Some(help),
                            );
                            res.register_error(&col.errors);
                            res
                        }
                        other => ::toml_pretty_deser::TomlValue::new_wrong_type(other, parent_span, "string"),
                    }
                }
            }
        }
    }

    pub fn gen_partial_struct_fields(
        fields: &syn::punctuated::Punctuated<syn::Field, syn::Token![,]>,
    ) -> Vec<TokenStream2> {
        fields
            .iter()
            .filter_map(|f| {
                // Skipped fields with tpd_skip(false) are NOT included in the partial struct
                // They will just use Default::default() in to_concrete()
                if is_skipped_field(f) {
                    if is_skipped_field_available_in_verify(f) {
                        // tpd_skip or tpd_skip(true): include as Option<T> so they can be set in verify()
                        let name = &f.ident;
                        let ty = &f.ty;
                        return Some(quote! {
                            #name: Option<#ty>
                        });
                    } else {
                        // tpd_skip(false): exclude from partial struct entirely
                        return None;
                    }
                }
                let name = &f.ident;
                let ty = &f.ty;

                if is_indexmap_type(ty) || is_option_indexmap_type(ty) {
                    let is_optional = is_option_indexmap_type(ty);
                    let (key_ty, value_ty) = if is_optional {
                        (
                            extract_option_indexmap_key_type(ty).expect("Can't fail"),
                            extract_option_indexmap_value_type(ty).expect("Can't fail"),
                        )
                    } else {
                        (
                            extract_indexmap_key_type(ty).expect("Can't fail"),
                            extract_indexmap_value_type(ty).expect("Can't fail"),
                        )
                    };
                    let kind = analyze_indexmap_value_type(&value_ty, f);

                    Some(match kind {
                        IndexMapValueKind::Nested(partial_type) => {
                            if is_optional {
                                quote! { #name: TomlValue<Option<indexmap::IndexMap<#key_ty, #partial_type>>> }
                            } else {
                                quote! { #name: TomlValue<indexmap::IndexMap<#key_ty, #partial_type>> }
                            }
                        }
                        IndexMapValueKind::VecNested(partial_type) => {
                            if is_optional {
                                quote! { #name: TomlValue<Option<indexmap::IndexMap<#key_ty, Vec<#partial_type>>>> }
                            } else {
                                quote! { #name: TomlValue<indexmap::IndexMap<#key_ty, Vec<#partial_type>>> }
                            }
                        }
                        IndexMapValueKind::Primitive => {
                            if is_optional {
                                quote! { #name: TomlValue<#ty> }
                            } else {
                                quote! { #name: TomlValue<#ty> }
                            }
                        }
                    })
                } else if is_nested_field(f) {
                    Some(if is_option_vec_type(ty) {
                        // Option<Vec<nested>>
                        let partial_type = make_partial_option_vec_inner_type(ty).expect("can't fail");
                        quote! {
                            #name: TomlValue<Option<Vec<#partial_type>>>
                        }
                    } else if is_option_box_type(ty) {
                        // Option<Box<nested>> - the partial is the inner T, not the Box
                        let partial_type = make_partial_option_box_inner_type(ty).expect("can't fail");
                        quote! {
                            #name: TomlValue<Option<#partial_type>>
                        }
                    } else if is_option_type(ty) {
                        let partial_type = make_partial_option_inner_type(ty).expect("can't fail");
                        quote! {
                            #name: TomlValue<Option<#partial_type>>
                        }
                    } else if is_vec_type(ty) {
                        let partial_type = make_partial_vec_inner_type(ty).expect("can't fail");
                        quote! {
                            #name: TomlValue<Vec<#partial_type>>
                        }
                    } else if is_box_type(ty) {
                        // Box<nested> - the partial is the inner T, not the Box
                        let partial_type = make_partial_box_inner_type(ty).expect("can't fail");
                        quote! {
                            #name: TomlValue<#partial_type>
                        }
                    } else {
                        let partial_type = make_partial_type_path(ty).expect("can't fail");
                        quote! {
                            #name: TomlValue<#partial_type>
                        }
                    })
                } else {
                    Some(quote! {
                        #name: TomlValue<#ty>
                    })
                }
            })
            .collect()
    }

    pub fn gen_from_toml_table_complete_impl(
        struct_name: &syn::Ident,
        partial_name: &syn::Ident,
        fields: &syn::punctuated::Punctuated<syn::Field, syn::Token![,]>,
        generics: &syn::Generics,
    ) -> TokenStream2 {
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        // Generate code that pushes field name to failing_fields if the field fails can_concrete check
        // For nested fields, also collect nested failing fields with a prefix
        let can_concrete_checks: Vec<_> = fields
            .iter()
            .filter(|f| !is_skipped_field(f))
            .map(|f| {
                let name = &f.ident;
                let name_str = name.as_ref().expect("field must have name").to_string();
                let ty = &f.ty;

                if is_skipped_field(f) {
                    // Skipped fields always pass - no check needed
                    quote! {}
                } else if is_absorb_remaining_field(f) {
                    // absorb_remaining fields always pass can_concrete (empty map is valid)
                    quote! {}
                } else if is_defaulted_field(f) && is_nested_field(f) {
                    // tpd_default + tpd_nested: true if missing (will use default) or if nested can_concrete
                    quote! {
                        if let Some(p) = self.#name.value.as_ref() {
                            let nested = p.can_concrete();
                            for nested_field in nested {
                                failing_fields.push(format!("{}.{}", #name_str, nested_field));
                            }
                        }
                    }
                } else if is_defaulted_field(f) {
                    // tpd_default fields always pass can_concrete (will use Default::default() if missing)
                    quote! {}
                } else if is_adapt_in_verify_field(f) {
                    // adapt_in_verify fields must be set by user in verify()
                    quote! {
                        if !self.#name.has_value() {
                            failing_fields.push(#name_str.to_string());
                        }
                    }
                } else if is_indexmap_type(ty) || is_option_indexmap_type(ty) {
                    let is_optional = is_option_indexmap_type(ty);
                    let value_ty = if is_optional {
                        extract_option_indexmap_value_type(ty).expect("can't fail")
                    } else {
                        extract_indexmap_value_type(ty).expect("can't fail")
                    };
                    let kind = analyze_indexmap_value_type(&value_ty, f);

                    match kind {
                        IndexMapValueKind::Nested(_) => {
                            if is_optional {
                                quote! {
                                    if let Some(Some(map)) = self.#name.value.as_ref() {
                                        for (key, p) in map.iter() {
                                            let nested = p.can_concrete();
                                            for nested_field in nested {
                                                failing_fields.push(format!("{}.{}.{}", #name_str, key, nested_field));
                                            }
                                        }
                                    }
                                }
                            } else {
                                quote! {
                                    if let Some(map) = self.#name.value.as_ref() {
                                        for (key, p) in map.iter() {
                                            let nested = p.can_concrete();
                                            for nested_field in nested {
                                                failing_fields.push(format!("{}.{}.{}", #name_str, key, nested_field));
                                            }
                                        }
                                    } else {
                                        failing_fields.push(#name_str.to_string());
                                    }
                                }
                            }
                        }
                        IndexMapValueKind::VecNested(_) => {
                            if is_optional {
                                quote! {
                                    if let Some(Some(map)) = self.#name.value.as_ref() {
                                        for (key, vec) in map.iter() {
                                            for (idx, p) in vec.iter().enumerate() {
                                                let nested = p.can_concrete();
                                                for nested_field in nested {
                                                    failing_fields.push(format!("{}.{}[{}].{}", #name_str, key, idx, nested_field));
                                                }
                                            }
                                        }
                                    }
                                }
                            } else {
                                quote! {
                                    if let Some(map) = self.#name.value.as_ref() {
                                        for (key, vec) in map.iter() {
                                            for (idx, p) in vec.iter().enumerate() {
                                                let nested = p.can_concrete();
                                                for nested_field in nested {
                                                    failing_fields.push(format!("{}.{}[{}].{}", #name_str, key, idx, nested_field));
                                                }
                                            }
                                        }
                                    } else {
                                        failing_fields.push(#name_str.to_string());
                                    }
                                }
                            }
                        }
                        IndexMapValueKind::Primitive => {
                            quote! {
                                if !self.#name.has_value() {
                                    failing_fields.push(#name_str.to_string());
                                }
                            }
                        }
                    }
                } else if is_nested_field(f) {
                    if is_option_vec_type(&f.ty) {
                        // Option<Vec<nested>>
                        quote! {
                            if let Some(Some(vec)) = self.#name.value.as_ref() {
                                for (idx, p) in vec.iter().enumerate() {
                                    let nested = p.can_concrete();
                                    for nested_field in nested {
                                        failing_fields.push(format!("{}[{}].{}", #name_str, idx, nested_field));
                                    }
                                }
                            }
                        }
                    } else if is_option_box_type(&f.ty) {
                        // Option<Box<nested>> - stored as Option<PartialT>
                        quote! {
                            if let Some(Some(p)) = self.#name.value.as_ref() {
                                let nested = p.can_concrete();
                                for nested_field in nested {
                                    failing_fields.push(format!("{}.{}", #name_str, nested_field));
                                }
                            }
                        }
                    } else if is_option_type(&f.ty) {
                        quote! {
                            if let Some(Some(p)) = self.#name.value.as_ref() {
                                let nested = p.can_concrete();
                                for nested_field in nested {
                                    failing_fields.push(format!("{}.{}", #name_str, nested_field));
                                }
                            }
                        }
                    } else if is_vec_type(&f.ty) {
                        quote! {
                            if let Some(vec) = self.#name.value.as_ref() {
                                for (idx, p) in vec.iter().enumerate() {
                                    let nested = p.can_concrete();
                                    for nested_field in nested {
                                        failing_fields.push(format!("{}[{}].{}", #name_str, idx, nested_field));
                                    }
                                }
                            } else {
                                failing_fields.push(#name_str.to_string());
                            }
                        }
                    } else if is_box_type(&f.ty) {
                        // Box<nested> - the partial is the inner T, so same as regular nested
                        quote! {
                            if let Some(p) = self.#name.value.as_ref() {
                                let nested = p.can_concrete();
                                for nested_field in nested {
                                    failing_fields.push(format!("{}.{}", #name_str, nested_field));
                                }
                            } else {
                                failing_fields.push(#name_str.to_string());
                            }
                        }
                    } else {
                        quote! {
                            if let Some(p) = self.#name.value.as_ref() {
                                let nested = p.can_concrete();
                                for nested_field in nested {
                                    failing_fields.push(format!("{}.{}", #name_str, nested_field));
                                }
                            } else {
                                failing_fields.push(#name_str.to_string());
                            }
                        }
                    }
                } else {
                    quote! {
                        if !self.#name.has_value() {
                            failing_fields.push(#name_str.to_string());
                        }
                    }
                }
            })
            .collect();

        let to_concrete_fields: Vec<_> = fields
            .iter()
            .map(|f| {
                let name = &f.ident;
                let ty = &f.ty;

                if is_skipped_field(f) {
                    if is_skipped_field_available_in_verify(f) {
                        // tpd_skip or tpd_skip(true): use the value set in verify(), or Default::default() if not set
                        quote! {
                            #name: self.#name.unwrap_or_default()
                        }
                    } else {
                        // tpd_skip(false): field not in partial struct, always use Default::default()
                        quote! {
                            #name: <#ty as ::std::default::Default>::default()
                        }
                    }
                } else if is_absorb_remaining_field(f) {
                    // absorb_remaining fields extract their value or use empty map if None
                    // Need to handle nested types specially
                    let is_optional = is_option_indexmap_type(ty);
                    let value_ty = if is_optional {
                        extract_option_indexmap_value_type(ty).expect("can't fail")
                    } else {
                        extract_indexmap_value_type(ty).expect("can't fail")
                    };
                    let kind = analyze_indexmap_value_type(&value_ty, f);
                    
                    match kind {
                        IndexMapValueKind::Nested(_) => {
                            // Convert each partial to concrete
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
                                    }).unwrap_or_default()
                                }
                            }
                        }
                        IndexMapValueKind::VecNested(_) => {
                            // Convert each Vec<partial> to Vec<concrete>
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
                                    }).unwrap_or_default()
                                }
                            }
                        }
                        IndexMapValueKind::Primitive => {
                            // Simple case - just extract the value
                            if is_optional {
                                quote! {
                                    #name: self.#name.value.flatten()
                                }
                            } else {
                                quote! {
                                    #name: self.#name.value.unwrap_or_default()
                                }
                            }
                        }
                    }
                } else if is_defaulted_field(f) && is_nested_field(f) {
                    // tpd_default + tpd_nested: use T::default() when missing, otherwise convert partial to concrete
                    quote! {
                        #name: self.#name.value
                            .and_then(|p| p.to_concrete())
                            .unwrap_or_else(|| <#ty as ::std::default::Default>::default())
                    }
                } else if is_defaulted_field(f) {
                    // tpd_default fields use Default::default() if missing
                    quote! {
                        #name: self.#name.value.unwrap_or_default()
                    }
                } else if is_adapt_in_verify_field(f) {
                    // adapt_in_verify fields are extracted like regular fields
                    quote! {
                        #name: self.#name.expect("was checked by can_concrete before")
                    }
                } else if is_indexmap_type(ty) || is_option_indexmap_type(ty) {
                    let is_optional = is_option_indexmap_type(ty);
                    let value_ty = if is_optional {
                        extract_option_indexmap_value_type(ty).expect("Failed to extract option type")
                    } else {
                        extract_indexmap_value_type(ty).expect("failed to extract index map type")
                    };
                    let kind = analyze_indexmap_value_type(&value_ty, f);

                    match kind {
                        IndexMapValueKind::Nested(_) => {
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
                        IndexMapValueKind::VecNested(_) => {
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
                        IndexMapValueKind::Primitive => {
                            quote! {
                                #name: self.#name.expect("was checked by can_concrete before")
                            }
                        }
                    }
                } else if is_nested_field(f) {
                    if is_option_vec_type(&f.ty) {
                        // Option<Vec<nested>>
                        quote! {
                            #name: self.#name.value.flatten().map(|vec| {
                                vec.into_iter().filter_map(|p| p.to_concrete()).collect()
                            })
                        }
                    } else if is_option_box_type(&f.ty) {
                        // Option<Box<nested>> - convert partial to concrete and wrap in Box
                        quote! {
                            #name: self.#name.value.flatten().and_then(|p| p.to_concrete()).map(Box::new)
                        }
                    } else if is_option_type(&f.ty) {
                        quote! {
                            #name: self.#name.value.flatten().and_then(|p| p.to_concrete())
                        }
                    } else if is_vec_type(&f.ty) {
                        quote! {
                            #name: self.#name.value.map(|vec| {
                                vec.into_iter().filter_map(|p| p.to_concrete()).collect()
                            }).expect("was checked by can_concrete before")
                        }
                    } else if is_box_type(&f.ty) {
                        // Box<nested> - convert partial to concrete and wrap in Box
                        quote! {
                            #name: Box::new(self.#name.value.and_then(|p| p.to_concrete()).expect("was checked by can_concrete before"))
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
            .filter(|f| !is_skipped_field(f) && !is_adapt_in_verify_field(f) && !is_absorb_remaining_field(f))
            .map(|f| {
                let name = &f.ident;
                let name_str = name.as_ref().expect("failed to get field name").to_string();
                let aliases = extract_aliases(f);
                let ty = &f.ty;

                // Check if this field has a tpd_with converter
                let with_func = extract_tpd_with_function(f);

                if is_indexmap_type(ty) || is_option_indexmap_type(ty) {
                    let is_optional = is_option_indexmap_type(ty);

                    if let Some(func) = with_func {
                        // tpd_with on IndexMap: apply converter to each value
                        if is_optional {
                            quote! {
                                #name: {
                                    let item_result: TomlValue<::toml_edit::Item> = helper.get_with_aliases(#name_str, &[#(#aliases),*], false);
                                    ::toml_pretty_deser::toml_item_as_map_with(&item_result, &helper.col, #func).into_optional()
                                }
                            }
                        } else {
                            quote! {
                                #name: {
                                    let item_result: TomlValue<::toml_edit::Item> = helper.get_with_aliases(#name_str, &[#(#aliases),*], true);
                                    ::toml_pretty_deser::toml_item_as_map_with(&item_result, &helper.col, #func)
                                }
                            }
                        }
                    } else if is_optional {
                        quote! {
                            #name: toml_item_as_map(
                                    &helper.get_with_aliases::<::toml_edit::Item>(#name_str, &[#(#aliases),*], false),
                                    &helper.col).into_optional()
                        }
                    } else {
                        quote! {
                            #name: toml_item_as_map(
                                    &helper.get_with_aliases::<::toml_edit::Item>(#name_str, &[#(#aliases),*], true),
                                    &helper.col)
                        }
                    }
                } else if is_vec_type(ty) {
                    let missing_is_error = !is_defaulted_field(f) && !is_defaulted_in_verify_field(f);
                    if let Some(func) = with_func {
                        // tpd_with on Vec<T>: apply converter to each element
                        quote! {
                            #name: {
                                let item_result: TomlValue<::toml_edit::Item> = helper.get_with_aliases(#name_str, &[#(#aliases),*], #missing_is_error);
                                ::toml_pretty_deser::toml_item_as_vec_with(&item_result, &helper.col, #func)
                            }
                        }
                    } else {
                        quote! {
                            #name: helper.get_with_aliases(#name_str, &[#(#aliases),*], #missing_is_error)
                        }
                    }
                } else if is_option_vec_type(ty) {
                    if let Some(func) = with_func {
                        // tpd_with on Option<Vec<T>>: apply converter to each element
                        quote! {
                            #name: {
                                let item_result: TomlValue<::toml_edit::Item> = helper.get_with_aliases(#name_str, &[#(#aliases),*], false);
                                ::toml_pretty_deser::toml_item_as_vec_with(&item_result, &helper.col, #func).into_optional()
                            }
                        }
                    } else {
                        quote! {
                            #name: {
                                let t: TomlValue<_> = helper.get_with_aliases(#name_str, &[#(#aliases),*], false);
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
                    }
                } else if is_option_box_type(ty) {
                    if let Some(func) = with_func {
                        // tpd_with on Option<Box<T>>: if present, apply converter and wrap in Box; if missing, None
                        quote! {
                            #name: {
                                let raw: TomlValue<String> = helper.get_with_aliases(#name_str, &[#(#aliases),*], false);
                                match &raw.state {
                                    TomlValueState::Missing { parent_span, .. } => {
                                        // Field is missing - Option is None
                                        TomlValue {
                                            value: Some(None),
                                            state: TomlValueState::Ok { span: parent_span.clone() },
                                        }
                                    }
                                    TomlValueState::Ok { span } => {
                                        let s = raw.value.as_ref().expect("Ok state must have value");
                                        match #func(s.as_str()) {
                                            Ok(converted) => TomlValue::new_ok(Some(Box::new(converted)), span.clone()),
                                            Err((msg, help)) => {
                                                let failed = TomlValue::new_validation_failed(span.clone(), msg, help);
                                                failed.register_error(&helper.col.errors);
                                                failed
                                            }
                                        }
                                    }
                                    _ => {
                                        // Wrong type or other error - forward it
                                        raw.register_error(&helper.col.errors);
                                        TomlValue {
                                            value: None,
                                            state: raw.state,
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        quote! {
                            #name: {
                                let t: TomlValue<_> = helper.get_with_aliases(#name_str, &[#(#aliases),*], false);
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
                    }
                } else if is_option_type(ty) {
                    if let Some(func) = with_func {
                        // tpd_with on Option<T>: if present, apply converter; if missing, None
                        quote! {
                            #name: {
                                let raw: TomlValue<String> = helper.get_with_aliases(#name_str, &[#(#aliases),*], false);
                                match &raw.state {
                                    TomlValueState::Missing { parent_span, .. } => {
                                        // Field is missing - Option is None
                                        TomlValue {
                                            value: Some(None),
                                            state: TomlValueState::Ok { span: parent_span.clone() },
                                        }
                                    }
                                    TomlValueState::Ok { span } => {
                                        let s = raw.value.as_ref().expect("Ok state must have value");
                                        match #func(s.as_str()) {
                                            Ok(converted) => TomlValue::new_ok(Some(converted), span.clone()),
                                            Err((msg, help)) => {
                                                let failed = TomlValue::new_validation_failed(span.clone(), msg, help);
                                                failed.register_error(&helper.col.errors);
                                                failed
                                            }
                                        }
                                    }
                                    _ => {
                                        // Wrong type or other error - forward it
                                        raw.register_error(&helper.col.errors);
                                        TomlValue {
                                            value: None,
                                            state: raw.state,
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        quote! {
                            #name: {
                                let t: TomlValue<_> = helper.get_with_aliases(#name_str, &[#(#aliases),*], false);
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
                    }
                } else if let Some(func) = with_func {
                    // Field has tpd_with converter
                    // The converter function takes &str and returns Result<T, (String, Option<String>)>
                    let missing_is_error = !is_defaulted_field(f) && !is_defaulted_in_verify_field(f);
                    let has_default = is_defaulted_field(f) || is_defaulted_in_verify_field(f);
                    let needs_box = is_box_type(ty);
                    
                    // Generate the wrapping expression based on whether Box is needed
                    let wrap_converted = if needs_box {
                        quote! { Box::new(converted) }
                    } else {
                        quote! { converted }
                    };
                    
                    if has_default {
                        // If field has default and is missing, use default instead of calling converter
                        quote! {
                            #name: {
                                let raw: TomlValue<String> = helper.get_with_aliases(#name_str, &[#(#aliases),*], #missing_is_error);
                                match &raw.state {
                                    TomlValueState::Missing { .. } => {
                                        // Field is missing - keep the Missing state
                                        // It will be defaulted in to_concrete()
                                        TomlValue {
                                            value: None,
                                            state: raw.state,
                                        }
                                    }
                                    TomlValueState::Ok { span } => {
                                        let s = raw.value.as_ref().expect("Ok state must have value");
                                        match #func(s.as_str()) {
                                            Ok(converted) => TomlValue::new_ok(#wrap_converted, span.clone()),
                                            Err((msg, help)) => {
                                                let failed = TomlValue::new_validation_failed(span.clone(), msg, help);
                                                failed.register_error(&helper.col.errors);
                                                failed
                                            }
                                        }
                                    }
                                    _ => {
                                        // Wrong type or other error - forward it
                                        raw.register_error(&helper.col.errors);
                                        TomlValue {
                                            value: None,
                                            state: raw.state,
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        quote! {
                            #name: {
                                let raw: TomlValue<String> = helper.get_with_aliases(#name_str, &[#(#aliases),*], #missing_is_error);
                                match &raw.state {
                                    TomlValueState::Ok { span } => {
                                        let s = raw.value.as_ref().expect("Ok state must have value");
                                        match #func(s.as_str()) {
                                            Ok(converted) => TomlValue::new_ok(#wrap_converted, span.clone()),
                                            Err((msg, help)) => {
                                                let failed = TomlValue::new_validation_failed(span.clone(), msg, help);
                                                failed.register_error(&helper.col.errors);
                                                failed
                                            }
                                        }
                                    }
                                    _ => {
                                        // Missing, wrong type, or other error - forward it
                                        raw.register_error(&helper.col.errors);
                                        TomlValue {
                                            value: None,
                                            state: raw.state,
                                        }
                                    }
                                }
                            }
                        }
                    }
                } else {
                    let missing_is_error = !is_defaulted_field(f) && !is_defaulted_in_verify_field(f);
                    quote! {
                        #name: helper.get_with_aliases(#name_str, &[#(#aliases),*], #missing_is_error)
                    }
                }
            })
            .collect();

        // adapt_in_verify fields are initialized with NotSet state
        let adapt_in_verify_fields: Vec<_> = fields
            .iter()
            .filter(|f| is_adapt_in_verify_field(f))
            .map(|f| {
                let name = &f.ident;
                quote! {
                    #name: TomlValue {
                        value: None,
                        state: TomlValueState::NotSet,
                    }
                }
            })
            .collect();

        // skipped fields with tpd_skip(true) are initialized with None (can be set in verify())
        // Fields with tpd_skip(false) are not included in the partial struct at all
        let skipped_fields: Vec<_> = fields
            .iter()
            .filter(|f| is_skipped_field(f) && is_skipped_field_available_in_verify(f))
            .map(|f| {
                let name = &f.ident;
                quote! {
                    #name: None
                }
            })
            .collect();

        // absorb_remaining fields collect all keys not matched by other fields
        let absorb_remaining_fields: Vec<_> = fields
            .iter()
            .filter(|f| is_absorb_remaining_field(f))
            .map(|f| {
                let name = &f.ident;
                let ty = &f.ty;
                let is_optional = is_option_indexmap_type(ty);
                let (key_ty, value_ty) = if is_optional {
                    (
                        extract_option_indexmap_key_type(ty).expect("Failed to extract option indexmap key type"),
                        extract_option_indexmap_value_type(ty).expect("Failed to extract option indexmap value type"),
                    )
                } else {
                    (
                        extract_indexmap_key_type(ty).expect("Failed to extract indexmap key type"),
                        extract_indexmap_value_type(ty).expect("Failed to extract indexmap value type"),
                    )
                };
                
                // Check if the value type needs to be a partial type (for nested)
                let kind = analyze_indexmap_value_type(&value_ty, f);
                
                match kind {
                    IndexMapValueKind::Nested(partial_type) => {
                        if is_optional {
                            quote! {
                                #name: helper.absorb_remaining::<#key_ty, #partial_type>().into_optional()
                            }
                        } else {
                            quote! {
                                #name: helper.absorb_remaining::<#key_ty, #partial_type>()
                            }
                        }
                    }
                    IndexMapValueKind::VecNested(partial_type) => {
                        // For Vec<Nested>, we still need the partial type
                        if is_optional {
                            quote! {
                                #name: helper.absorb_remaining::<#key_ty, Vec<#partial_type>>().into_optional()
                            }
                        } else {
                            quote! {
                                #name: helper.absorb_remaining::<#key_ty, Vec<#partial_type>>()
                            }
                        }
                    }
                    IndexMapValueKind::Primitive => {
                        if is_optional {
                            quote! {
                                #name: helper.absorb_remaining::<#key_ty, #value_ty>().into_optional()
                            }
                        } else {
                            quote! {
                                #name: helper.absorb_remaining::<#key_ty, #value_ty>()
                            }
                        }
                    }
                }
            })
            .collect();

        quote! {
            impl #impl_generics FromTomlTable<#struct_name> for #partial_name #ty_generics #where_clause {
                fn can_concrete(&self) -> Vec<String> {
                    let mut failing_fields: Vec<String> = Vec::new();
                    #(#can_concrete_checks)*
                    failing_fields
                }

                fn to_concrete(self) -> Option<#struct_name #ty_generics> {
                    Some(#struct_name {
                        #(#to_concrete_fields,)*
                    })
                }

                fn from_toml_table(helper: &mut TomlHelper<'_>) -> Self {
                    #partial_name {
                        #(#from_toml_table_fields,)*
                        #(#adapt_in_verify_fields,)*
                        #(#skipped_fields,)*
                        #(#absorb_remaining_fields,)*
                    }
                }
            }
        }
    }

    pub fn gen_from_toml_item_for_struct(
        partial_name: &syn::Ident,
        _generate_verify: bool,
        has_absorb_remaining: bool,
    ) -> TokenStream2 {
        // Always call verify() - either the macro generates a default impl,
        // or the user provides one with `partial = false`
        let verify_call = quote! { .verify(&mut helper) };

        // Only call deny_unknown if there's no absorb_remaining field
        // (absorb_remaining handles unknown keys by absorbing them)
        let deny_unknown_call = if has_absorb_remaining {
            quote! {}
        } else {
            quote! { helper.deny_unknown(); }
        };

        quote! {
            impl FromTomlItem for #partial_name {
                fn from_toml_item(item: &toml_edit::Item, parent_span: std::ops::Range<usize>,
                    col: &TomlCollector
                ) -> TomlValue<Self> {
                    match item.as_table_like_plus() {
                        Some(table) => {
                            let mut helper = TomlHelper::from_table(table,
                                col.clone());
                            let partial = #partial_name::from_toml_table(&mut helper)
                                #verify_call;
                            #deny_unknown_call

                            if partial.can_concrete().is_empty()  {
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
        }
    }

    pub fn gen_verify_from_toml_if_needed(
        partial_name: &syn::Ident,
        generics: &syn::Generics,
        generate_verify: bool,
        has_adapt_in_verify_fields: bool,
    ) -> TokenStream2 {
        // Don't generate default VerifyFromToml if:
        // - User specified partial = false
        // - Struct has adapt_in_verify fields (user MUST implement verify())
        if generate_verify && !has_adapt_in_verify_fields {
            let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

            quote! {
                impl #impl_generics VerifyFromToml for #partial_name #ty_generics #where_clause {
                    fn verify(self, _helper: &mut TomlHelper<'_>) -> Self {
                        self
                    }
                }
            }
        } else {
            quote! {}
        }
    }

    pub fn gen_adapt_in_verify_getters(
        partial_name: &syn::Ident,
        fields: &syn::punctuated::Punctuated<syn::Field, syn::Token![,]>,
        generics: &syn::Generics,
    ) -> TokenStream2 {
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        let getters: Vec<_> = fields
            .iter()
            .filter(|f| is_adapt_in_verify_field(f))
            .map(|f| {
                let name = f.ident.as_ref().expect("failed to get field name 2");
                let getter_name = format_ident!("tpd_get_{}", name);
                let name_str = name.to_string();
                let aliases = extract_aliases(f);

                // If tpd_default or tpd_default_in_verify is also present, 
                // force missing_is_error to false regardless of what the caller passes
                let has_default = is_defaulted_field(f) || is_defaulted_in_verify_field(f);
                let missing_is_error_expr = if has_default {
                    quote! { false }
                } else {
                    quote! { missing_is_error }
                };

                quote! {
                    pub fn #getter_name<T: FromTomlItem + std::fmt::Debug>(
                        &self,
                        helper: &mut TomlHelper<'_>,
                        missing_is_error: bool,
                        auto_register_type_errors: bool,
                    ) -> TomlValue<T> {
                        if auto_register_type_errors {
                            helper.get_with_aliases(#name_str, &[#(#aliases),*], #missing_is_error_expr)
                        } else {
                            helper.get_with_aliases_no_auto_error(#name_str, &[#(#aliases),*], #missing_is_error_expr)
                        }
                    }
                }
            })
            .collect();

        if getters.is_empty() {
            quote! {}
        } else {
            quote! {
                impl #impl_generics #partial_name #ty_generics #where_clause {
                    #(#getters)*
                }
            }
        }
    }

    pub fn gen_tagged_enum_variants(variants: &[syn::Variant]) -> Vec<TokenStream2> {
        variants
            .iter()
            .map(|v| {
                let variant_name = &v.ident;
                match &v.fields {
                    Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                        let inner_ty = &fields.unnamed.first().expect("gen_tagged_enum_variants - failed to get first unmapped field").ty;
                        // For Box<T>, the partial is PartialT (not Box<PartialT>)
                        let partial_inner_type = if is_box_type(inner_ty) {
                            make_partial_box_inner_type(inner_ty).expect("gen_tagged_enum_variants - failed to create partial type path for Box inner type")
                        } else {
                            make_partial_type_path(inner_ty).expect("gen_tagged_enum_variants - failed to create partial type path for first unmapped field")
                        };
                        quote_spanned! { v.span() =>
                            #variant_name(#partial_inner_type)
                        }
                    }
                    _ => panic!("tagged enum only supports single unnamed field variants"),
                }
            })
            .collect()
    }

    pub fn gen_tagged_enum_meta_impl(
        partial_name: &syn::Ident,
        generics: &syn::Generics,
        variants: &[syn::Variant],
        tag_key: &str,
        tag_aliases: &[String],
    ) -> TokenStream2 {
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        let variant_names: Vec<_> = variants.iter().map(|v| v.ident.to_string()).collect();

        // Collect all names including aliases for suggestion purposes
        let all_names_with_aliases: Vec<_> = variants
            .iter()
            .flat_map(|v| {
                let primary = v.ident.to_string();
                let aliases = extract_variant_aliases(v);
                std::iter::once(primary).chain(aliases)
            })
            .collect();

        // Generate arms for variant_aliases function
        let variant_aliases_arms: Vec<_> = variants
            .iter()
            .map(|v| {
                let variant_name_str = v.ident.to_string();
                let aliases = extract_variant_aliases(v);
                quote! {
                    #variant_name_str => &[#(#aliases),*]
                }
            })
            .collect();

        let deserialize_variant_arms: Vec<_> = variants
            .iter()
            .map(|v| {
                let variant_name = &v.ident;
                let variant_name_str = variant_name.to_string();
                match &v.fields {
                    Fields::Unnamed(fields) => {
                        let inner_ty = &fields.unnamed.first().expect("failed to get first unampped field - gen_tagged_enum_meta_impl").ty;
                        // For Box<T>, the partial is PartialT (not Box<PartialT>)
                        let partial_inner_type = if is_box_type(inner_ty) {
                            make_partial_box_inner_type(inner_ty).expect("gen_tagged_enum_meta_impl - failed to create partial type path for Box inner type")
                        } else {
                            make_partial_type_path(inner_ty).expect("gen_tagged_enum_meta_impl - failed to create partial type path for first unnamed field")
                        };
                        quote_spanned! { v.span() =>
                            #variant_name_str => {
                                let result: ::toml_pretty_deser::TomlValue<#partial_inner_type> =
                                        ::toml_pretty_deser::deserialize_nested(
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

        quote! {
            impl #impl_generics ::toml_pretty_deser::TaggedEnumMeta for #partial_name #ty_generics #where_clause {
                const TAG_KEY: &'static str = #tag_key;
                const TAG_ALIASES: &'static [&'static str] = &[#(#tag_aliases),*];

                fn all_variant_names() -> &'static [&'static str] {
                    &[#(#variant_names),*]
                }

                fn all_variant_names_with_aliases() -> &'static [&'static str] {
                    &[#(#all_names_with_aliases),*]
                }

                fn variant_aliases(variant_name: &str) -> &'static [&'static str] {
                    match variant_name {
                        #(#variant_aliases_arms,)*
                        _ => &[],
                    }
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
        }
    }

    pub fn gen_tagged_enum_from_toml_table(
        enum_name: &syn::Ident,
        partial_name: &syn::Ident,
        generics: &syn::Generics,
        variants: &[syn::Variant],
    ) -> TokenStream2 {
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        let can_concrete_variants: Vec<_> = variants
            .iter()
            .map(|v| {
                let variant_name = &v.ident;
                quote_spanned! { v.span() =>
                    #partial_name::#variant_name(inner) => inner.can_concrete(),
                }
            })
            .collect();

        let to_concrete_variants: Vec<_> = variants
            .iter()
            .map(|v| {
                let variant_name = &v.ident;
                // Check if the original type was Box<T>
                let is_boxed = match &v.fields {
                    Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                        let inner_ty = &fields.unnamed.first().expect("to_concrete_variants").ty;
                        is_box_type(inner_ty)
                    }
                    _ => false,
                };
                if is_boxed {
                    quote_spanned! { v.span() =>
                        #partial_name::#variant_name(inner) => {
                            inner.to_concrete().map(|c| #enum_name::#variant_name(Box::new(c)))
                        }
                    }
                } else {
                    quote_spanned! { v.span() =>
                        #partial_name::#variant_name(inner) => {
                            inner.to_concrete().map(#enum_name::#variant_name)
                        }
                    }
                }
            })
            .collect();

        quote! {
            impl #impl_generics FromTomlTable<#enum_name> for #partial_name #ty_generics #where_clause {
                fn can_concrete(&self) -> Vec<String> {
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
        }
    }

    pub fn gen_tagged_enum_from_toml_item_for_partial(
        enum_name: &syn::Ident,
        partial_name: &syn::Ident,
        generics: &syn::Generics,
        tag_key: &str,
        tag_aliases: &[String],
    ) -> TokenStream2 {
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        quote_spanned! { enum_name.span() =>
            impl #impl_generics FromTomlItem for #partial_name #ty_generics #where_clause {
                fn from_toml_item(item: &toml_edit::Item, parent_span: std::ops::Range<usize>,
                    col: &TomlCollector
            ) -> TomlValue<Self> {
                    let variant_names = Self::all_variant_names();
                    let all_names_with_aliases = Self::all_variant_names_with_aliases();
                    match item.as_table_like_plus() {
                    Some(table) => {
                        let mut helper = TomlHelper::from_item(item, col);
                        let tag_key = #tag_key;
                        let tag_aliases = &[#(#tag_aliases),*];
                        let tag_result: TomlValue<String> =
                                helper.get_with_aliases(tag_key, tag_aliases, false);
                        let mut fields_to_ignore: Vec<&str> = vec![tag_key];
                        fields_to_ignore.extend(tag_aliases.iter().copied());
                        let span: std::ops::Range<usize> = table.span().unwrap_or(0..0);
                        match &tag_result.state {
                            TomlValueState::Ok { span: tag_span, } => {
                                let tag_str = tag_result.value.as_ref().expect("No avlue on TomlValueState::Ok. Bug");
                                let mut matched_variant: Option<&str> = None;

                                // First try to match against primary variant names
                                for variant_name in variant_names {
                                    if col.match_mode.matches(variant_name, tag_str) {
                                        matched_variant = Some(variant_name);
                                        break;
                                    }
                                }

                                // If no match, try to match against aliases
                                if matched_variant.is_none() {
                                    for variant_name in variant_names {
                                        let aliases = Self::variant_aliases(variant_name);
                                        for alias in aliases {
                                            if col.match_mode.matches(alias, tag_str) {
                                                matched_variant = Some(variant_name);
                                                break;
                                            }
                                        }
                                        if matched_variant.is_some() {
                                            break;
                                        }
                                    }
                                }

                                if let Some(variant_name) = matched_variant {
                                    match Self::deserialize_variant(
                                        variant_name,
                                        item,
                                        col,
                                        &fields_to_ignore,
                                    ) {
                                        Some(partial) => TomlValue {
                                            value: Some(partial),
                                            state: TomlValueState::Ok { span: tag_span.clone()},
                                        },
                                        None => TomlValue {
                                            value: None,
                                            state: TomlValueState::ValidationFailed {
                                                span: tag_span.clone(),
                                                message: "Failed to deserialize variant".to_string(),
                                                help: None,
                                            },
                                        },
                                    }
                                } else {
                                    TomlValue {
                                        value: None,
                                        state: TomlValueState::ValidationFailed {
                                            span: tag_span.clone(),
                                            message: "Unknown enum variant".to_string(),
                                            help: Some(suggest_alternatives(tag_str, all_names_with_aliases)),
                                        },
                                    }
                                }
                            }
                            TomlValueState::MultiDefined { key: _, spans } => TomlValue {
                                value: None,
                                state: TomlValueState::MultiDefined {
                                    key: tag_key.to_string(),
                                    spans: spans.clone(),
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
                                    help: Some(suggest_alternatives("", all_names_with_aliases)),
                                },
                            },
                            TomlValueState::Missing { .. } => TomlValue {
                                value: None,
                                state: TomlValueState::ValidationFailed {
                                    span,
                                    message: format!("Missing required tag field: {}", tag_key),
                                    help: Some(suggest_alternatives("", all_names_with_aliases)),
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
        }
    }

    pub fn gen_from_toml_item_for_tuple_struct(
        struct_name: &syn::Ident,
        inner_type: &Type,
        generics: &syn::Generics,
    ) -> TokenStream2 {
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        quote_spanned! { struct_name.span() =>
            impl #impl_generics FromTomlItem for #struct_name #ty_generics #where_clause {
                fn from_toml_item(item: &toml_edit::Item, parent_span: std::ops::Range<usize>,
                    col: &TomlCollector
                ) -> TomlValue<Self> {
                    let inner_value: TomlValue<#inner_type> = FromTomlItem::from_toml_item(item, parent_span, col);
                    match inner_value.value {
                        Some(inner) => TomlValue {
                            value: Some(#struct_name(inner)),
                            state: inner_value.state,
                        },
                        None => TomlValue {
                            value: None,
                            state: inner_value.state,
                        },
                    }
                }
            }
        }
    }

    pub fn gen_from_toml_item_for_concrete_enum(
        enum_name: &syn::Ident,
        partial_name: &syn::Ident,
        generics: &syn::Generics,
        tag_key: &str,
        tag_aliases: &[String],
    ) -> TokenStream2 {
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        quote_spanned! { enum_name.span() =>
             impl #impl_generics FromTomlItem for #enum_name #ty_generics #where_clause {
                fn from_toml_item(item: &toml_edit::Item, parent_span: std::ops::Range<usize>,
                    col: &TomlCollector
            ) -> TomlValue<Self> {
                    let variant_names = #partial_name::all_variant_names();
                    let all_names_with_aliases = #partial_name::all_variant_names_with_aliases();
                    match item.as_table_like_plus() {
                    Some(table) => {
                        let mut helper = TomlHelper::from_item(item, col);
                        let tag_key = #tag_key;
                        let tag_aliases = &[#(#tag_aliases),*];
                        let tag_result: TomlValue<String> =
                                helper.get_with_aliases(tag_key, tag_aliases, true);
                        let mut fields_to_ignore: Vec<&str> = vec![tag_key];
                        fields_to_ignore.extend(tag_aliases.iter().copied());
                        let span: std::ops::Range<usize> = table.span().unwrap_or(0..0);
                        match &tag_result.state {
                            TomlValueState::Ok { .. } => {
                                let tag_str = tag_result.value.as_ref().expect("No value on TomlValueState::Ok. Bug");
                                let mut matched_variant: Option<&str> = None;

                                // First try to match against primary variant names
                                for variant_name in variant_names {
                                    if col.match_mode.matches(variant_name, tag_str) {
                                        matched_variant = Some(variant_name);
                                        break;
                                    }
                                }

                                // If no match, try to match against aliases
                                if matched_variant.is_none() {
                                    for variant_name in variant_names {
                                        let aliases = #partial_name::variant_aliases(variant_name);
                                        for alias in aliases {
                                            if col.match_mode.matches(alias, tag_str) {
                                                matched_variant = Some(variant_name);
                                                break;
                                            }
                                        }
                                        if matched_variant.is_some() {
                                            break;
                                        }
                                    }
                                }

                                if let Some(variant_name) = matched_variant {
                                    match #partial_name::deserialize_variant(
                                        variant_name,
                                        item,
                                        col,
                                        &fields_to_ignore,
                                    ) {
                                        Some(partial) => {
                                            if partial.can_concrete().is_empty() {
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
                                            help: Some(suggest_alternatives(tag_str, all_names_with_aliases)),
                                        },
                                    }
                                }
                            }
                            TomlValueState::MultiDefined { key: _, spans } => TomlValue {
                                value: None,
                                state: TomlValueState::MultiDefined {
                                    key: tag_key.to_string(),
                                    spans: spans.clone(),
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
                                    help: Some(suggest_alternatives("", all_names_with_aliases)),
                                },
                            },
                            TomlValueState::Missing { .. } => TomlValue {
                                value: None,
                                state: TomlValueState::ValidationFailed {
                                    span,
                                    message: format!("Missing required tag field: {}", tag_key),
                                    help: Some(suggest_alternatives("", all_names_with_aliases)),
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
        }
    }
}

/// Module for specialized handlers
mod handlers {
    use super::*;
    use crate::codegen::*;
    use crate::type_analysis::*;

    pub fn handle_unit_enum(input: DeriveInput) -> TokenStream {
        let mut cleaned_input = input.clone();
        clean_enum_input(&mut cleaned_input);

        let enum_name = &input.ident;
        let generics = &input.generics;
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        let variants = match &input.data {
            Data::Enum(data) => &data.variants,
            _ => unreachable!("handle_unit_enum called on non-enum"),
        };

        for v in variants {
            if !is_unit_enum_variant(v) {
                panic!(
                    "unit enum only supports unit variants (no fields). Variant '{}' has fields.",
                    v.ident
                );
            }
        }

        let variants_slice: Vec<_> = variants.iter().cloned().collect();
        let string_named_enum_impl = gen_string_named_enum_impl(
            enum_name,
            &impl_generics,
            &ty_generics,
            where_clause,
            &variants_slice,
        );
        let from_toml_item_impl =
            gen_from_toml_item_for_unit_enum(enum_name, &impl_generics, &ty_generics, where_clause, &variants_slice);

        let expanded = quote! {
            #cleaned_input

            #string_named_enum_impl
            #from_toml_item_impl
        };

        TokenStream::from(expanded)
    }

    pub fn handle_tuple_struct(input: DeriveInput) -> TokenStream {
        let struct_name = &input.ident;
        let generics = &input.generics;

        let inner_type = match &input.data {
            Data::Struct(data) => match &data.fields {
                Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                    &fields
                        .unnamed
                        .first()
                        .expect("handle_tuple_struct failed to get first unmapped field")
                        .ty
                }
                _ => panic!("tuple struct only supports structs with a single unnamed field"),
            },
            _ => panic!("tuple struct only supports structs"),
        };

        let from_toml_item_impl =
            gen_from_toml_item_for_tuple_struct(struct_name, inner_type, generics);

        let expanded = quote! {
            #input

            #from_toml_item_impl
        };

        TokenStream::from(expanded)
    }

    pub fn handle_struct(input: DeriveInput, generate_verify: bool) -> TokenStream {
        let struct_name = &input.ident;
        let partial_name = format_ident!("Partial{}", struct_name);
        let vis = &input.vis;
        let generics = &input.generics;
        let (_impl_generics, _ty_generics, where_clause) = generics.split_for_impl();

        let fields = match &input.data {
            Data::Struct(data) => match &data.fields {
                Fields::Named(fields) => &fields.named,
                _ => panic!("struct only supports structs with named fields"),
            },
            _ => panic!("struct only supports structs"),
        };

        let mut cleaned_input = input.clone();
        clean_struct_input(&mut cleaned_input);

        // Validate absorb_remaining field(s)
        let absorb_remaining_fields: Vec<_> = fields
            .iter()
            .filter(|f| is_absorb_remaining_field(f))
            .collect();

        if absorb_remaining_fields.len() > 1 {
            panic!(
                "Only one field can have #[tpd_absorb_remaining]. Found {} fields with this attribute.",
                absorb_remaining_fields.len()
            );
        }

        // Validate absorb_remaining field type: must be IndexMap<K, T> or Option<IndexMap<K, T>>
        // Key type K must implement From<String> (validated at trait bound level, not here)
        for f in &absorb_remaining_fields {
            let ty = &f.ty;
            let field_name = f.ident.as_ref().map_or_else(|| "unnamed".to_string(), |i| i.to_string());

            if !is_option_indexmap_type(ty) && !is_indexmap_type(ty) {
                // Wrong type entirely
                panic!(
                    "#[tpd_absorb_remaining] on field '{}' requires type IndexMap<K, T> or Option<IndexMap<K, T>>, found: {}",
                    field_name, quote!(#ty)
                );
            }
        }

        let has_absorb_remaining = !absorb_remaining_fields.is_empty();

        for f in fields {
            let ty = &f.ty;
            let is_indexmap = is_indexmap_type(ty) || is_option_indexmap_type(ty);

            if is_nested_field(f) && !is_indexmap {
                if is_option_vec_type(ty) {
                    // Option<Vec<nested>> - check that we can extract the inner type
                    assert!(
                        extract_option_vec_inner_type(ty).is_some(),
                        "nested attribute on Option<Vec<_>> field {:?} requires a simple inner type name",
                        &f.ident
                    );
                } else if is_option_type(ty) {
                    assert!(
                        extract_option_inner_type(ty).is_some(),
                        "nested attribute on Option field {:?} requires a simple inner type name",
                        &f.ident
                    );
                } else if is_vec_type(ty) {
                    assert!(
                        extract_vec_inner_type(ty).is_some(),
                        "nested attribute on Vec field requires a simple inner type name"
                    );
                } else if extract_type_name(ty).is_none() {
                    panic!("nested attribute requires a simple type name");
                }
            }
        }

        let partial_fields = gen_partial_struct_fields(fields);
        let from_toml_table_complete_impl =
            gen_from_toml_table_complete_impl(struct_name, &partial_name, fields, generics);
        let from_toml_item_impl = gen_from_toml_item_for_struct(&partial_name, generate_verify, has_absorb_remaining);
        let has_adapt_in_verify_fields = fields.iter().any(is_adapt_in_verify_field);
        let verify_from_toml_impl = gen_verify_from_toml_if_needed(
            &partial_name,
            generics,
            generate_verify,
            has_adapt_in_verify_fields,
        );
        let adapt_in_verify_getters = gen_adapt_in_verify_getters(&partial_name, fields, generics);

        let expanded = quote! {
            #cleaned_input

            #[derive(Debug)]
            #generics
            #vis struct #partial_name #generics #where_clause {
                #(#partial_fields,)*
            }

            #from_toml_table_complete_impl
            #from_toml_item_impl
            #verify_from_toml_impl
            #adapt_in_verify_getters
        };

        TokenStream::from(expanded)
    }

    #[allow(clippy::too_many_lines)]
    pub fn handle_tagged_enum(
        input: DeriveInput,
        tag_key: String,
        aliases: Vec<String>,
    ) -> TokenStream {
        let mut cleaned_input = input.clone();
        clean_enum_input(&mut cleaned_input);

        let enum_name = &input.ident;
        let partial_name = format_ident!("Partial{}", enum_name);
        let vis = &input.vis;
        let generics = &input.generics;
        let (_impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        let variants = match &input.data {
            Data::Enum(data) => &data.variants,
            _ => panic!("tagged enum only supports enums"),
        };

        for v in variants {
            if !is_single_unnamed_field_variant(v) {
                panic!(
                    "tagged enum only supports single unnamed field variants. Variant '{}' does not have a single unnamed field.",
                    v.ident
                );
            }
        }

        let variants_slice: Vec<_> = variants.iter().cloned().collect();
        let partial_variants = gen_tagged_enum_variants(&variants_slice);
        let tagged_enum_meta_impl =
            gen_tagged_enum_meta_impl(&partial_name, generics, &variants_slice, &tag_key, &aliases);
        let from_toml_table_impl =
            gen_tagged_enum_from_toml_table(enum_name, &partial_name, generics, &variants_slice);
        let from_toml_item_partial_impl = gen_tagged_enum_from_toml_item_for_partial(
            enum_name,
            &partial_name,
            generics,
            &tag_key,
            &aliases,
        );
        let from_toml_item_concrete_impl = gen_from_toml_item_for_concrete_enum(
            enum_name,
            &partial_name,
            generics,
            &tag_key,
            &aliases,
        );

        let expanded = quote_spanned! { enum_name.span() =>
            #cleaned_input

            #[derive(Debug)]
            #generics
            #vis enum #partial_name #ty_generics #where_clause {
                #(#partial_variants,)*
            }

            #tagged_enum_meta_impl
            #from_toml_table_impl
            #from_toml_item_partial_impl
            #from_toml_item_concrete_impl
        };

        TokenStream::from(expanded)
    }
}
