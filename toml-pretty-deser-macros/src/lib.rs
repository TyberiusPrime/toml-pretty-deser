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
/// - **Unit enums**: Implements `StringNamedEnum` and `FromTomlItem` for enum variants
/// - **Tagged enums**: Generates `Partial{Enum}` with `TaggedEnumMeta` implementation
///
/// # Examples
///
/// ## Structs
///
/// ```ignore
/// #[tdp]                                          // With VerifyFromToml impl
/// #[tdp(partial = false)]                         // Without VerifyFromToml impl
/// #[derive(Debug)]
/// struct Config {
///     name: String,
///     count: u32,
///     #[nested]
///     nested: InnerStruct,
/// }
/// ```
///
/// ## Unit Enums
///
/// ```ignore
/// #[tdp]
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
/// #[tdp(tag = "kind")]                            // Basic tagged enum
/// #[tdp(tag = "kind", aliases = ["type"])]        // With tag aliases
/// #[derive(Debug)]
/// enum EitherOne {
///     KindA(InnerA),
///     KindB(InnerB),
/// }
/// ```
#[proc_macro_attribute]
pub fn tdp(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = if attr.is_empty() {
        TdpArgs::default()
    } else {
        parse_macro_input!(attr as TdpArgs)
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
            _ => panic!("#[tdp] only supports structs with named fields"),
        },
        Data::Enum(data) => {
            let has_tag = args.tag.is_some();
            let all_unit = data.variants.iter().all(|v| matches!(v.fields, Fields::Unit));
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
                    panic!("For tagged enums, you must specify tag = \"key\". Example: #[tdp(tag = \"kind\")]");
                }
                _ => panic!(
                    "#[tdp] only supports:\n\
                     - Structs with named fields\n\
                     - Unit enums (all variants without fields)\n\
                     - Tagged enums (all variants with single unnamed field) - requires tag argument\n\n\
                     Your enum has mixed or unsupported field types."
                ),
            }
        }
        Data::Union(_) => panic!("#[tdp] does not support unions"),
    }
}

/// Arguments for the `#[tdp]` attribute
#[derive(Default)]
struct TdpArgs {
    partial: Option<bool>,
    tag: Option<String>,
    aliases: Vec<String>,
}

impl syn::parse::Parse for TdpArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut args = TdpArgs::default();

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
                    ))
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
                !attr.path().is_ident("nested")
                    && !attr.path().is_ident("enum_tagged")
                    && !attr.path().is_ident("tpd_alias")
                    && !attr.path().is_ident("tpd_default_in_verify")
            });
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
            .any(|attr| attr.path().is_ident("nested"))
    }

    pub fn is_defaulted_field(field: &syn::Field) -> bool {
        field
            .attrs
            .iter()
            .any(|attr| attr.path().is_ident("tpd_default_in_verify"))
    }

    pub fn is_enum_tagged_field(field: &syn::Field) -> bool {
        field
            .attrs
            .iter()
            .any(|attr| attr.path().is_ident("enum_tagged"))
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

    pub fn extract_type_name(ty: &Type) -> Option<syn::Ident> {
        match ty {
            Type::Path(TypePath { path, .. }) => path.get_ident().cloned(),
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
        Nested(syn::Ident),
        VecNested(syn::Ident),
    }

    pub fn analyze_indexmap_value_type(value_ty: &Type, field: &syn::Field) -> IndexMapValueKind {
        let is_nested = is_nested_field(field);
        let is_enum_tagged = is_enum_tagged_field(field);

        if let Some(vec_inner) = extract_vec_inner_full_type(value_ty) {
            if (is_nested | is_enum_tagged)
                && let Some(ident) = extract_type_name(&vec_inner)
            {
                return IndexMapValueKind::VecNested(ident);
            }
        }

        if (is_nested | is_enum_tagged)
            && let Some(ident) = extract_type_name(value_ty)
        {
            return IndexMapValueKind::Nested(ident);
        }

        IndexMapValueKind::Primitive
    }

    pub fn is_unit_enum_variant(variant: &syn::Variant) -> bool {
        matches!(variant.fields, Fields::Unit)
    }

    pub fn is_single_unnamed_field_variant(variant: &syn::Variant) -> bool {
        matches!(&variant.fields, Fields::Unnamed(f) if f.unnamed.len() == 1)
    }
}

/// Module for code generation functions
mod codegen {
    use super::*;
    use proc_macro2::TokenStream as TokenStream2;
    use crate::type_analysis::*;
    use syn::spanned::Spanned;

    pub fn gen_string_named_enum_impl(
        enum_name: &syn::Ident,
        impl_generics: &syn::ImplGenerics,
        ty_generics: &syn::TypeGenerics,
        where_clause: Option<&syn::WhereClause>,
        variants: &[syn::Variant],
    ) -> TokenStream2 {
        let variant_names: Vec<_> = variants.iter().map(|v| v.ident.to_string()).collect();

        let from_str_arms = variants.iter().map(|v| {
            let name = &v.ident;
            let name_str = name.to_string();
            quote! {
                #name_str => ::std::option::Option::Some(#enum_name::#name)
            }
        });

        quote! {
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
        }
    }

    pub fn gen_from_toml_item_for_unit_enum(
        enum_name: &syn::Ident,
        impl_generics: &syn::ImplGenerics,
        ty_generics: &syn::TypeGenerics,
        where_clause: Option<&syn::WhereClause>,
    ) -> TokenStream2 {
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
        }
    }

    pub fn gen_partial_struct_fields(
        fields: &syn::punctuated::Punctuated<syn::Field, syn::Token![,]>,
    ) -> Vec<TokenStream2> {
        fields
            .iter()
            .map(|f| {
                let name = &f.ident;
                let ty = &f.ty;

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
                        IndexMapValueKind::VecNested(inner_name) => {
                            let partial_type = format_ident!("Partial{}", inner_name);
                            if is_optional {
                                quote! { #name: TomlValue<Option<indexmap::IndexMap<String, Vec<#partial_type>>>> }
                            } else {
                                quote! { #name: TomlValue<indexmap::IndexMap<String, Vec<#partial_type>>> }
                            }
                        }
                        IndexMapValueKind::Primitive => {
                            if is_optional {
                                quote! { #name: TomlValue<#ty> }
                            } else {
                                quote! { #name: TomlValue<#ty> }
                            }
                        }
                    }
                } else if is_nested_field(f) {
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
                } else if is_enum_tagged_field(f) {
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
            .collect()
    }

    pub fn gen_can_concrete_impl(
        struct_name: &syn::Ident,
        partial_name: &syn::Ident,
        fields: &syn::punctuated::Punctuated<syn::Field, syn::Token![,]>,
        generics: &syn::Generics,
    ) -> TokenStream2 {
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        let can_concrete_fields: Vec<_> = fields
            .iter()
            .map(|f| {
                let name = &f.ident;
                let ty = &f.ty;

                if is_indexmap_type(ty) || is_option_indexmap_type(ty) {
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
                        IndexMapValueKind::VecNested(_) => {
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
                        IndexMapValueKind::Primitive => {
                            quote! {
                                self.#name.has_value()
                            }
                        }
                    }
                } else if is_nested_field(f) {
                    if is_option_type(&f.ty) {
                        quote! {
                            self.#name.value.as_ref().map(|opt| {
                                opt.as_ref().map(|p| p.can_concrete()).unwrap_or(true)
                            }).unwrap_or(false)
                        }
                    } else if is_vec_type(&f.ty) {
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
                    if is_option_type(&f.ty) {
                        quote! {
                            self.#name.value.as_ref().map(|opt| {
                                opt.as_ref().map(|p| p.can_concrete()).unwrap_or(true)
                            }).unwrap_or(false)
                        }
                    } else if is_vec_type(&f.ty) {
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

        quote! {
            impl #impl_generics FromTomlTable<#struct_name> for #partial_name #ty_generics #where_clause {
                fn can_concrete(&self) -> bool {
                    #(#can_concrete_fields)&&*
                }
            }
        }
    }

    pub fn gen_to_concrete_impl(
        struct_name: &syn::Ident,
        partial_name: &syn::Ident,
        fields: &syn::punctuated::Punctuated<syn::Field, syn::Token![,]>,
        generics: &syn::Generics,
    ) -> TokenStream2 {
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        let to_concrete_fields: Vec<_> = fields
            .iter()
            .map(|f| {
                let name = &f.ident;
                let ty = &f.ty;

                if is_indexmap_type(ty) || is_option_indexmap_type(ty) {
                    let is_optional = is_option_indexmap_type(ty);
                    let value_ty = if is_optional {
                        extract_option_indexmap_value_type(ty).unwrap()
                    } else {
                        extract_indexmap_value_type(ty).unwrap()
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
                    if is_option_type(&f.ty) {
                        quote! {
                            #name: self.#name.value.flatten().and_then(|p| p.to_concrete())
                        }
                    } else if is_vec_type(&f.ty) {
                        quote! {
                            #name: self.#name.value.map(|vec| {
                                vec.into_iter().filter_map(|p| p.to_concrete()).collect()
                            }).expect("was checked by can_concrete before")
                        }
                    } else {
                        quote! {
                            #name: self.#name.value.and_then(|p| p.to_concrete()).expect("was checked by can_concrete before")
                        }
                    }
                } else if is_enum_tagged_field(f) {
                    if is_option_type(&f.ty) {
                        quote! {
                            #name: self.#name.value.flatten().and_then(|p| p.to_concrete())
                        }
                    } else if is_vec_type(&f.ty) {
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

        quote! {
            impl #impl_generics FromTomlTable<#struct_name> for #partial_name #ty_generics #where_clause {
                fn to_concrete(self) -> Option<#struct_name #ty_generics> {
                    Some(#struct_name {
                        #(#to_concrete_fields,)*
                    })
                }
            }
        }
    }

    pub fn gen_from_toml_table_complete_impl(
        struct_name: &syn::Ident,
        partial_name: &syn::Ident,
        fields: &syn::punctuated::Punctuated<syn::Field, syn::Token![,]>,
        generics: &syn::Generics,
    ) -> TokenStream2 {
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        let can_concrete_fields: Vec<_> = fields
            .iter()
            .map(|f| {
                let name = &f.ident;
                let ty = &f.ty;

                if is_indexmap_type(ty) || is_option_indexmap_type(ty) {
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
                        IndexMapValueKind::VecNested(_) => {
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
                        IndexMapValueKind::Primitive => {
                            quote! {
                                self.#name.has_value()
                            }
                        }
                    }
                } else if is_nested_field(f) {
                    if is_option_type(&f.ty) {
                        quote! {
                            self.#name.value.as_ref().map(|opt| {
                                opt.as_ref().map(|p| p.can_concrete()).unwrap_or(true)
                            }).unwrap_or(false)
                        }
                    } else if is_vec_type(&f.ty) {
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
                    if is_option_type(&f.ty) {
                        quote! {
                            self.#name.value.as_ref().map(|opt| {
                                opt.as_ref().map(|p| p.can_concrete()).unwrap_or(true)
                            }).unwrap_or(false)
                        }
                    } else if is_vec_type(&f.ty) {
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

                if is_indexmap_type(ty) || is_option_indexmap_type(ty) {
                    let is_optional = is_option_indexmap_type(ty);
                    let value_ty = if is_optional {
                        extract_option_indexmap_value_type(ty).unwrap()
                    } else {
                        extract_indexmap_value_type(ty).unwrap()
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
                    if is_option_type(&f.ty) {
                        quote! {
                            #name: self.#name.value.flatten().and_then(|p| p.to_concrete())
                        }
                    } else if is_vec_type(&f.ty) {
                        quote! {
                            #name: self.#name.value.map(|vec| {
                                vec.into_iter().filter_map(|p| p.to_concrete()).collect()
                            }).expect("was checked by can_concrete before")
                        }
                    } else {
                        quote! {
                            #name: self.#name.value.and_then(|p| p.to_concrete()).expect("was checked by can_concrete before")
                        }
                    }
                } else if is_enum_tagged_field(f) {
                    if is_option_type(&f.ty) {
                        quote! {
                            #name: self.#name.value.flatten().and_then(|p| p.to_concrete())
                        }
                    } else if is_vec_type(&f.ty) {
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

                if is_indexmap_type(ty) || is_option_indexmap_type(ty) {
                    let is_optional = is_option_indexmap_type(ty);

                    if is_optional {
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
                } else if is_option_type(ty) {
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
                } else {
                    let missing_is_error = !is_defaulted_field(f);
                    quote! {
                        #name: helper.get_with_aliases(#name_str, &[#(#aliases),*], #missing_is_error)
                    }
                }
            })
            .collect();

        quote! {
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
        }
    }

    pub fn gen_from_toml_table_impl(
        struct_name: &syn::Ident,
        partial_name: &syn::Ident,
        fields: &syn::punctuated::Punctuated<syn::Field, syn::Token![,]>,
        generics: &syn::Generics,
    ) -> TokenStream2 {
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        let from_toml_table_fields: Vec<_> = fields
            .iter()
            .map(|f| {
                let name = &f.ident;
                let name_str = name.as_ref().unwrap().to_string();
                let aliases = extract_aliases(f);
                let ty = &f.ty;

                if is_indexmap_type(ty) || is_option_indexmap_type(ty) {
                    let is_optional = is_option_indexmap_type(ty);

                    if is_optional {
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
                } else if is_option_type(ty) {
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
                } else {
                    let missing_is_error = !is_defaulted_field(f);
                    quote! {
                        #name: helper.get_with_aliases(#name_str, &[#(#aliases),*], #missing_is_error)
                    }
                }
            })
            .collect();

        quote! {
            impl #impl_generics FromTomlTable<#struct_name> for #partial_name #ty_generics #where_clause {
                fn from_toml_table(helper: &mut TomlHelper<'_>) -> Self {
                    #partial_name {
                        #(#from_toml_table_fields,)*
                    }
                }
            }
        }
    }

    pub fn gen_from_toml_item_for_struct(
        partial_name: &syn::Ident,
        generate_verify: bool,
    ) -> TokenStream2 {
        let verify_call = if generate_verify {
            quote! { .verify(&mut helper) }
        } else {
            quote! {}
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
        }
    }

    pub fn gen_verify_from_toml_if_needed(
        partial_name: &syn::Ident,
        generics: &syn::Generics,
        generate_verify: bool,
    ) -> TokenStream2 {
        if generate_verify {
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

    pub fn gen_tagged_enum_variants(
        variants: &[syn::Variant],
        partial_name: &syn::Ident,
    ) -> Vec<TokenStream2> {
        variants
            .iter()
            .map(|v| {
                let variant_name = &v.ident;
                match &v.fields {
                    Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                        let inner_ty = &fields.unnamed.first().unwrap().ty;
                        let inner_type_name = extract_type_name(inner_ty).unwrap();
                        let partial_inner_type = format_ident!("Partial{}", inner_type_name);
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
        enum_name: &syn::Ident,
        partial_name: &syn::Ident,
        generics: &syn::Generics,
        variants: &[syn::Variant],
        tag_key: &str,
        tag_aliases: &[String],
    ) -> TokenStream2 {
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        let variant_names: Vec<_> = variants
            .iter()
            .map(|v| v.ident.to_string())
            .collect();

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
                quote_spanned! { v.span() =>
                    #partial_name::#variant_name(inner) => {
                        inner.to_concrete().map(#enum_name::#variant_name)
                    }
                }
            })
            .collect();

        quote! {
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
                            TomlValueState::Ok { .. } => {
                                let tag_str = tag_result.value.as_ref().expect("No avlue on TomlValueState::Ok. Bug");
                                let mut matched_variant: Option<&str> = None;

                                for variant_name in variant_names {
                                    if col.match_mode.matches(variant_name, tag_str) {
                                        matched_variant = Some(variant_name);
                                        break;
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

                                for variant_name in variant_names {
                                    if col.match_mode.matches(variant_name, tag_str) {
                                        matched_variant = Some(variant_name);
                                        break;
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
        }
    }
}

/// Module for specialized handlers
mod handlers {
    use super::*;
    use crate::type_analysis::*;
    use crate::codegen::*;

    pub fn handle_unit_enum(input: DeriveInput) -> TokenStream {
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
        let string_named_enum_impl =
            gen_string_named_enum_impl(enum_name, &impl_generics, &ty_generics, where_clause, &variants_slice);
        let from_toml_item_impl = gen_from_toml_item_for_unit_enum(enum_name, &impl_generics, &ty_generics, where_clause);

        let expanded = quote! {
            #input

            #string_named_enum_impl
            #from_toml_item_impl
        };

        TokenStream::from(expanded)
    }

    pub fn handle_struct(input: DeriveInput, generate_verify: bool) -> TokenStream {
        let struct_name = &input.ident;
        let partial_name = format_ident!("Partial{}", struct_name);
        let generics = &input.generics;
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        let fields = match &input.data {
            Data::Struct(data) => match &data.fields {
                Fields::Named(fields) => &fields.named,
                _ => panic!("struct only supports structs with named fields"),
            },
            _ => panic!("struct only supports structs"),
        };

        let mut cleaned_input = input.clone();
        clean_struct_input(&mut cleaned_input);

        for f in fields {
            let ty = &f.ty;
            let is_indexmap = is_indexmap_type(ty) || is_option_indexmap_type(ty);

            if is_nested_field(f) && !is_indexmap {
                if is_option_type(ty) {
                    assert!(
                        extract_option_inner_type(ty).is_some(),
                        "nested attribute on Option field requires a simple inner type name"
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
            if is_enum_tagged_field(f) && !is_indexmap {
                if is_option_type(ty) {
                    assert!(
                        extract_option_inner_type(ty).is_some(),
                        "enum_tagged attribute on Option field requires a simple inner type name"
                    );
                } else if is_vec_type(ty) {
                    assert!(
                        extract_vec_inner_type(ty).is_some(),
                        "enum_tagged attribute on Vec field requires a simple inner type name"
                    );
                } else if extract_type_name(ty).is_none() {
                    panic!("enum_tagged attribute requires a simple type name");
                }
            }
        }

        let partial_fields = gen_partial_struct_fields(fields);
        let from_toml_table_complete_impl = gen_from_toml_table_complete_impl(struct_name, &partial_name, fields, generics);
        let from_toml_item_impl = gen_from_toml_item_for_struct(&partial_name, generate_verify);
        let verify_from_toml_impl = gen_verify_from_toml_if_needed(&partial_name, generics, generate_verify);

        let expanded = quote! {
            #cleaned_input

            #[derive(Debug)]
            #generics
            struct #partial_name #generics #where_clause {
                #(#partial_fields,)*
            }

            #from_toml_table_complete_impl
            #from_toml_item_impl
            #verify_from_toml_impl
        };

        TokenStream::from(expanded)
    }

    #[allow(clippy::too_many_lines)]
    pub fn handle_tagged_enum(
        input: DeriveInput,
        tag_key: String,
        aliases: Vec<String>,
    ) -> TokenStream {
        let enum_name = &input.ident;
        let partial_name = format_ident!("Partial{}", enum_name);
        let generics = &input.generics;
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

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
        let partial_variants = gen_tagged_enum_variants(&variants_slice, &partial_name);
        let tagged_enum_meta_impl = gen_tagged_enum_meta_impl(
            enum_name,
            &partial_name,
            generics,
            &variants_slice,
            &tag_key,
            &aliases,
        );
        let from_toml_table_impl =
            gen_tagged_enum_from_toml_table(enum_name, &partial_name, generics, &variants_slice);
        let from_toml_item_partial_impl = gen_tagged_enum_from_toml_item_for_partial(
            enum_name,
            &partial_name,
            generics,
            &tag_key,
            &aliases,
        );
        let from_toml_item_concrete_impl =
            gen_from_toml_item_for_concrete_enum(enum_name, &partial_name, generics, &tag_key, &aliases);

        let expanded = quote_spanned! { enum_name.span() =>
            #input

            #[derive(Debug)]
            #generics
            enum #partial_name #ty_generics #where_clause {
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
