use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    Data, DeriveInput, Fields, GenericArgument, Lit, PathArguments, Type, TypePath,
    parse_macro_input,
};

/// Attribute macro for generating TOML deserialization code.
///
/// # Usage
/// - `#[tpd(root)]` on struct - Top-level deserialization target
/// - `#[tpd(root, no_verify)]` on struct - Root with blanket `VerifyIn`
/// - `#[tpd]` on struct - Nested struct (user must provide `VerifyIn` impls)
/// - `#[tpd(no_verify)]` on struct - Nested struct with blanket `VerifyIn`
/// - `#[tpd(tag = "key")]` on enum - Tagged enum
/// - `#[tpd]` on enum - Simple string enum
///
/// # Field-level attributes
/// - `#[tpd(nested)]` - Type has a Partial variant (nested struct or tagged enum)
/// - `#[tpd(alias("name1", "name2"))]` - Field aliases
/// - `#[tpd(skip)]` - Skip field, use `Default::default()` in `into_concrete`
/// - `#[tpd(default)]` - Use `Default::default()` if field is missing from TOML
/// - `#[tpd(with = "func")]` - Adapter function
/// - `#[tpd(absorb_remaining)]` - Absorb all remaining unmatched fields
///
/// # Variant-level attributes (simple enum)
/// - `#[tpd(alias("name1", "name2"))]` - Variant aliases
///
///
/// # Panics
///
/// - When used on an incompatible enum
/// - When nested structs are not tagged #[tpd(nested)]
#[proc_macro_attribute]
pub fn tpd(attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    let attr_args = attr.to_string();
    match &input.data {
        Data::Struct(_) => derive_struct(&input, &attr_args),
        Data::Enum(_) => derive_enum(&input, &attr_args),
        Data::Union(_) => panic!("tpd cannot be applied to unions"),
    }
}

// ============================================================================
// Attribute parsing
// ============================================================================

fn is_root_attr(attr_args: &str) -> bool {
    attr_args.contains("root")
}

fn is_no_verify_attr(attr_args: &str) -> bool {
    attr_args.contains("no_verify")
}

/// Strip #[tpd(...)] attributes from a `DeriveInput` and return the cleaned item as tokens
fn emit_original_item(input: &DeriveInput) -> proc_macro2::TokenStream {
    let mut cleaned = input.clone();
    cleaned.attrs.retain(|a| !a.path().is_ident("tpd"));
    match &mut cleaned.data {
        Data::Struct(data) => {
            if let Fields::Named(named) = &mut data.fields {
                for field in &mut named.named {
                    field.attrs.retain(|a| !a.path().is_ident("tpd"));
                }
            }
        }
        Data::Enum(data) => {
            for variant in &mut data.variants {
                variant.attrs.retain(|a| !a.path().is_ident("tpd"));
                match &mut variant.fields {
                    Fields::Named(named) => {
                        for field in &mut named.named {
                            field.attrs.retain(|a| !a.path().is_ident("tpd"));
                        }
                    }
                    Fields::Unnamed(unnamed) => {
                        for field in &mut unnamed.unnamed {
                            field.attrs.retain(|a| !a.path().is_ident("tpd"));
                        }
                    }
                    Fields::Unit => {}
                }
            }
        }
        Data::Union(_) => {}
    }
    quote! { #cleaned }
}

#[derive(Default)]
enum NestedState {
    #[default]
    NotNested,
    Nested,
    Tagged,
}

#[derive(Default)]
struct FieldAttrs {
    nested: NestedState,
    aliases: Vec<String>,
    skip: bool,
    default: bool,
    with_fn: Option<String>,
    absorb_remaining: bool,
}

impl FieldAttrs {
    fn has_partial(&self) -> bool {
        matches!(self.nested, NestedState::Nested | NestedState::Tagged)
    }
}

fn parse_field_attrs(field: &syn::Field) -> FieldAttrs {
    let mut attrs = FieldAttrs::default();
    for attr in &field.attrs {
        if !attr.path().is_ident("tpd") {
            continue;
        }
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("nested") {
                attrs.nested = NestedState::Nested;
            } else if meta.path.is_ident("tagged") {
                attrs.nested = NestedState::Tagged;
            } else if meta.path.is_ident("skip") {
                attrs.skip = true;
            } else if meta.path.is_ident("default") {
                attrs.default = true;
            } else if meta.path.is_ident("absorb_remaining") {
                attrs.absorb_remaining = true;
            } else if meta.path.is_ident("with") {
                let value = meta.value().expect("No value on with");
                let lit: Lit = value.parse().expect("unparsable value on with");
                if let Lit::Str(s) = lit {
                    attrs.with_fn = Some(s.value());
                }
            } else if meta.path.is_ident("alias") {
                let content;
                syn::parenthesized!(content in meta.input);
                let lits =
                    syn::punctuated::Punctuated::<Lit, syn::Token![,]>::parse_terminated(&content)
                        .expect("failed to parse alias");
                for lit in lits {
                    if let Lit::Str(s) = lit {
                        attrs.aliases.push(s.value());
                    }
                }
            }
            Ok(())
        })
        .expect("failed to parse nested meta");
    }

      
    assert!(!(attrs.has_partial() && attrs.with_fn.is_some()),
        "Field cannot have both #[tpd(nested)] and #[tpd(with = \"...\")]");

    attrs
}

// ============================================================================
// Type analysis
// ============================================================================

#[derive(Clone)]
enum TypeKind {
    Leaf(Type),
    Optional(Box<TypeKind>),
    Vector(Box<TypeKind>),
    Map(Type, Box<TypeKind>),
    Boxed(Box<TypeKind>),
}

fn analyze_type(ty: &Type) -> TypeKind {
    if let Type::Path(TypePath { path, .. }) = ty {
        if let Some(last) = path.segments.last() {
            let name = last.ident.to_string();
            match name.as_str() {
                "Option" => {
                    let inner = extract_single_generic(&last.arguments);
                    TypeKind::Optional(Box::new(analyze_type(&inner)))
                }
                "Vec" => {
                    let inner = extract_single_generic(&last.arguments);
                    TypeKind::Vector(Box::new(analyze_type(&inner)))
                }
                "IndexMap" => {
                    let (key, value) = extract_two_generics(&last.arguments);
                    TypeKind::Map(key, Box::new(analyze_type(&value)))
                }
                "Box" => {
                    let inner = extract_single_generic(&last.arguments);
                    TypeKind::Boxed(Box::new(analyze_type(&inner)))
                }
                _ => TypeKind::Leaf(ty.clone()),
            }
        } else {
            TypeKind::Leaf(ty.clone())
        }
    } else {
        TypeKind::Leaf(ty.clone())
    }
}

fn extract_single_generic(args: &PathArguments) -> Type {
    if let PathArguments::AngleBracketed(ab) = args {
        if let Some(GenericArgument::Type(ty)) = ab.args.first() {
            return ty.clone();
        }
    }
    panic!("Expected single generic argument");
}

fn extract_two_generics(args: &PathArguments) -> (Type, Type) {
    if let PathArguments::AngleBracketed(ab) = args {
        let mut iter = ab.args.iter();
        if let (Some(GenericArgument::Type(k)), Some(GenericArgument::Type(v))) =
            (iter.next(), iter.next())
        {
            return (k.clone(), v.clone());
        }
    }
    panic!("Expected two generic arguments");
}

fn leaf_type_name(ty: &Type) -> String {
    if let Type::Path(TypePath { path, .. }) = ty {
        if let Some(last) = path.segments.last() {
            return last.ident.to_string();
        }
    }
    panic!(
        "Cannot extract type name from {:?}",
        quote!(#ty).to_string()
    );
}

/// Generate the partial type for a field's inner type (inside the outer `TomlValue` wrapper)
fn gen_partial_inner_type(kind: &TypeKind, is_nested: bool) -> proc_macro2::TokenStream {
    match kind {
        TypeKind::Leaf(ty) => {
            if is_nested {
                let name = leaf_type_name(ty);
                let partial_name = format_ident!("Partial{}", name);
                quote! { #partial_name }
            } else {
                quote! { #ty }
            }
        }
        TypeKind::Optional(inner) => {
            let inner_ty = gen_partial_inner_type(inner, is_nested);
            quote! { Option<#inner_ty> }
        }
        TypeKind::Vector(inner) => {
            let inner_ty = gen_partial_inner_type(inner, is_nested);
            quote! { Vec<toml_pretty_deser::TomlValue<#inner_ty>> }
        }
        TypeKind::Map(key, inner) => {
            let inner_ty = gen_partial_inner_type(inner, is_nested);
            quote! { indexmap::IndexMap<#key, toml_pretty_deser::TomlValue<#inner_ty>> }
        }
        TypeKind::Boxed(inner) => {
            let inner_ty = gen_partial_inner_type(inner, is_nested);
            quote! { Box<#inner_ty> }
        }
    }
}

/// Check if a type is the unit type `()`
fn is_unit_type(kind: &TypeKind) -> bool {
    if let TypeKind::Leaf(ty) = kind {
        if let Type::Tuple(tuple) = ty {
            return tuple.elems.is_empty();
        }
    }
    false
}

/// Check if `into_concrete()` needs to be called on the unwrapped value
fn needs_into_concrete(kind: &TypeKind, has_partial: bool) -> bool {
    match kind {
        TypeKind::Leaf(_) => has_partial,
        TypeKind::Vector(_) | TypeKind::Map(_, _) => true,
        TypeKind::Optional(inner) | TypeKind::Boxed(inner) => {
            needs_into_concrete(inner, has_partial)
        }
    }
}


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

// ============================================================================
// Struct derivation
// ============================================================================

#[allow(clippy::too_many_lines)]
fn derive_struct(input: &DeriveInput, attr_args: &str) -> TokenStream {
    struct FieldInfo {
        ident: syn::Ident,
        attrs: FieldAttrs,
        kind: TypeKind,
        vis: syn::Visibility,
        ty: Type,
    }

    let original_item = emit_original_item(&input);
    let is_root = is_root_attr(attr_args);
    let no_verify = is_no_verify_attr(attr_args);
    let name = &input.ident;
    let partial_name = format_ident!("Partial{}", name);
    let vis = &input.vis;

    let fields = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(named) => &named.named,
            _ => panic!("Tpd only supports named fields"),
        },
        _ => unreachable!(),
    };


    let field_infos: Vec<FieldInfo> = fields
        .iter()
        .map(|f| {
            let ident = f.ident.clone().expect("ident clone failed");
            let attrs = parse_field_attrs(f);
            let kind = analyze_type(&f.ty);

            if attrs.absorb_remaining &&
                !is_indexmap_type(&f.ty) 
            {
                // Wrong type entirely
                panic!(
                    "#[tpd_absorb_remaining] on field '{}' requires type IndexMap<FromString<_>, Visitor<>_> , T>>, found: {}",
                    ident,
                    quote!(#f.ty)
                );
            }

            FieldInfo {
                ident,
                attrs,
                kind,
                vis: f.vis.clone(),
                ty: f.ty.clone(),
            }
        })
        .collect();

    let absorb_count = field_infos.iter().filter(|f| f.attrs.absorb_remaining).count();
    assert!(absorb_count <= 1, 
                "Only one field can have #[tpd_absorb_remaining]. Found {absorb_count} fields with this attribute.",
            );

    // --- Partial struct fields (skip #[tpd(skip)] fields) ---
    let partial_fields: Vec<proc_macro2::TokenStream> = field_infos
        .iter()
        //.filter(|f| !f.attrs.skip)
        .map(|f| {
            let ident = &f.ident;
            let fvis = &f.vis;
            let ftype = &f.ty;
            if f.attrs.skip {
                quote! { #fvis #ident: Option<#ftype> }
            } else {
                let inner_ty = gen_partial_inner_type(&f.kind, f.attrs.has_partial());
                quote! { #fvis #ident: toml_pretty_deser::TomlValue<#inner_ty> }
            } 
        })
        .collect();

    // --- Getter methods ---
    let getter_methods: Vec<proc_macro2::TokenStream> = field_infos
        .iter()
        .filter(|f| !f.attrs.skip && !f.attrs.absorb_remaining)
        .map(|f| {
            let ident = &f.ident;
            let getter_name = format_ident!("tpd_get_{}", ident);
            let field_name_str = ident.to_string();
            let inner_ty = gen_partial_inner_type(&f.kind, f.attrs.has_partial());

            // Unit type () fields: no TOML lookup
            if is_unit_type(&f.kind) {
                return quote! {
                    fn #getter_name(&self, _helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<()> {
                        toml_pretty_deser::TomlValue::new_ok((), 0..0)
                    }
                };
            }

            let aliases_expr = if f.attrs.aliases.is_empty() {
                quote! { &[] }
            } else {
                let aliases = &f.attrs.aliases;
                quote! { &[#(#aliases),*] }
            };

            let is_option = matches!(&f.kind, TypeKind::Optional(_));

            if let Some(ref with_fn) = f.attrs.with_fn {
                let with_fn_ident: syn::Path = syn::parse_str(with_fn).expect("with fn_indent parse_str failed");
                quote! {
                    fn #getter_name(&self, helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<#inner_ty> {
                        #with_fn_ident(helper.get_with_aliases(#field_name_str, #aliases_expr))
                    }
                }
            } else if f.attrs.default {
                quote! {
                    fn #getter_name(&self, helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<#inner_ty> {
                        helper.get_with_aliases(#field_name_str, #aliases_expr).or_default()
                    }
                }
            } else if is_option {
                quote! {
                    fn #getter_name(&self, helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<#inner_ty> {
                        helper.get_with_aliases(#field_name_str, #aliases_expr).into_optional()
                    }
                }
            } else {
                quote! {
                    fn #getter_name(&self, helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<#inner_ty> {
                        helper.get_with_aliases(#field_name_str, #aliases_expr)
                    }
                }
            }
        })
        .collect();

    // --- fill_from_toml statements ---
    let fill_stmts: Vec<(proc_macro2::TokenStream, bool)> = field_infos
        .iter()
        .filter(|f| !f.attrs.skip)
        .map(|f| {
            let ident = &f.ident;
            let is_absorb = f.attrs.absorb_remaining;
            let stmt = if is_absorb {
                quote! { partial.#ident = helper.absorb_remaining(); }
            } else {
                let getter_name = format_ident!("tpd_get_{}", ident);
                quote! { partial.#ident = partial.#getter_name(helper); }
            };
            (stmt, is_absorb)
        })
        .collect();

    let regular_fill_stmts: Vec<&proc_macro2::TokenStream> = fill_stmts
        .iter()
        .filter(|(_, is_absorb)| !is_absorb)
        .map(|(s, _)| s)
        .collect();
    let absorb_fill_stmts: Vec<&proc_macro2::TokenStream> = fill_stmts
        .iter()
        .filter(|(_, is_absorb)| *is_absorb)
        .map(|(s, _)| s)
        .collect();

    // --- can_concrete checks ---
    let can_concrete_checks: Vec<proc_macro2::TokenStream> = field_infos
        .iter()
        .filter(|f| !f.attrs.skip)
        .map(|f| {
            let ident = &f.ident;
            quote! { self.#ident.is_ok() }
        })
        .collect();

    // --- v_register_errors ---
    let register_errors_calls: Vec<proc_macro2::TokenStream> = field_infos
        .iter()
        .filter(|f| !f.attrs.skip)
        .map(|f| {
            let ident = &f.ident;
            quote! { self.#ident.register_error(col); }
        })
        .collect();

    // --- into_concrete fields ---
    let into_concrete_fields: Vec<proc_macro2::TokenStream> = field_infos
        .iter()
        .map(|f| {
            let ident = &f.ident;
            if f.attrs.skip {
                quote! { #ident: self.#ident.unwrap_or_default() }
            } else if is_unit_type(&f.kind) {
                quote! { #ident: () }
            } else if needs_into_concrete(&f.kind, f.attrs.has_partial()) {
                quote! { #ident: self.#ident.value.expect("into concrete when can_concrete returned false").into_concrete() }
            } else {
                quote! { #ident: self.#ident.value.expect("into concrete when can_concrete returned false") }
            }
        })
        .collect();

    // --- vv_validate statements ---
    let vv_validate_stmts: Vec<proc_macro2::TokenStream> = field_infos
        .iter()
        .filter(|f| !f.attrs.skip && !is_unit_type(&f.kind))
        .map(|f| {
            let ident = &f.ident;
            quote! { self.#ident = self.#ident.take().tpd_validate(helper, &self); }
        })
        .collect();

    // --- is_table check for non-root ---
    let is_table_check = if is_root {
        quote! {}
    } else {
        quote! {
            if !helper.is_table() {
                return toml_pretty_deser::TomlValue::new_wrong_type(helper.item, helper.span(), "table or inline table");
            }
        }
    }; 

    // --- no_verify impl ---
    let no_verify_impl = if no_verify {
        quote! {
            impl<__TpdR> toml_pretty_deser::VerifyIn<__TpdR> for #partial_name {}
        }
    } else {
        quote! {}
    };

    // --- root tpd_from_toml ---
    let root_impl = if is_root {
        quote! {
            impl #name {
                #vis fn tpd_from_toml(
                    toml_str: &str,
                    field_match_mode: toml_pretty_deser::FieldMatchMode,
                    vec_mode: toml_pretty_deser::VecMode,
                ) -> Result<#name, toml_pretty_deser::DeserError<#partial_name>> {
                    toml_pretty_deser::helpers::deserialize_toml::<#partial_name>(toml_str, field_match_mode, vec_mode)
                }
            }
        }
    } else {
        quote! {}
    };

    let output = quote! {
        #original_item

        #[derive(Default, Debug)]
        #vis struct #partial_name {
            #(#partial_fields,)*
        }

        impl #partial_name {
            #(#getter_methods)*
        }

        impl toml_pretty_deser::Visitor for #partial_name {
            type Concrete = #name;

            fn fill_from_toml(helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<Self> {
                #is_table_check
                let mut partial = Self::default();
                #(#regular_fill_stmts)*
                #(#absorb_fill_stmts)*
                toml_pretty_deser::TomlValue::from_visitor(partial, helper)
            }

            fn can_concrete(&self) -> bool {
                #(#can_concrete_checks)&&*
            }

            fn v_register_errors(&self, col: &toml_pretty_deser::TomlCollector) {
                #(#register_errors_calls)*
            }

            fn into_concrete(self) -> #name {
                #name {
                    #(#into_concrete_fields,)*
                }
            }
        }

        impl<__TpdR> toml_pretty_deser::VerifyVisitor<__TpdR> for #partial_name {
            fn vv_validate(mut self, helper: &mut toml_pretty_deser::TomlHelper<'_>, _parent: &__TpdR) -> Self
            where
                Self: Sized + toml_pretty_deser::Visitor,
            {
                #(#vv_validate_stmts)*
                self
            }
        }

        #no_verify_impl

        #root_impl
    };

    output.into()
}

// ============================================================================
// Enum derivation
// ============================================================================

/// Parse tag = "key" from attribute args string like `tag = "kind"`
fn parse_tag_from_attr(attr_args: &str) -> Option<String> {
    let attr_args = attr_args.trim();
    if attr_args.is_empty() {
        return None;
    }
    // Parse: tag = "value"
    let meta: syn::Meta = syn::parse_str(&format!("tpd({attr_args})")).expect("Could not parse attr meta");
    if let syn::Meta::List(list) = meta {
        let nested: syn::MetaNameValue = syn::parse2(list.tokens).expect(&format!("Failed to parse '{attr_args}'"));
        if nested.path.is_ident("tag") {
            if let syn::Expr::Lit(syn::ExprLit {
                lit: Lit::Str(s), ..
            }) = &nested.value
            {
                return Some(s.value());
            }
        }
    }
    None
}

struct VariantAttrs {
    aliases: Vec<String>,
}

fn parse_variant_attrs(variant: &syn::Variant) -> VariantAttrs {
    let mut attrs = VariantAttrs {
        aliases: Vec::new(),
    };
    for attr in &variant.attrs {
        if !attr.path().is_ident("tpd") {
            continue;
        }
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("alias") {
                let content;
                syn::parenthesized!(content in meta.input);
                let lits =
                    syn::punctuated::Punctuated::<Lit, syn::Token![,]>::parse_terminated(&content)
                        .expect("failed to parse alias");
                for lit in lits {
                    if let Lit::Str(s) = lit {
                        attrs.aliases.push(s.value());
                    }
                }
            }
            Ok(())
        })
        .expect("failed to parse nested meta - case 2");
    }
    attrs
}

fn derive_enum(input: &DeriveInput, attr_args: &str) -> TokenStream {
    let tag = parse_tag_from_attr(attr_args);

    if tag.is_some() {
        derive_tagged_enum(&input, &tag.expect("Failed to parse tag #[tdp(tag=\"name\")] from {attr_args}"))
    } else {
        derive_simple_enum(&input)
    }
}

fn derive_simple_enum(input: &DeriveInput) -> TokenStream {
    struct VariantInfo {
        ident: syn::Ident,
        attrs: VariantAttrs,
    }
    let original_item = emit_original_item(&input);
    let name = &input.ident;

    let variants = match &input.data {
        Data::Enum(data) => &data.variants,
        _ => unreachable!(),
    };


    let variant_infos: Vec<VariantInfo> = variants
        .iter()
        .map(|v| {
            assert!(
                v.fields.is_empty(),
                "Simple enums must have unit variants. Use #[tpd(tag = \"key\")] for tagged enums."
            );
            VariantInfo {
                ident: v.ident.clone(),
                attrs: parse_variant_attrs(v),
            }
        })
        .collect();

    // Generate match arms for string matching
    let match_arms: Vec<proc_macro2::TokenStream> = variant_infos
        .iter()
        .map(|v| {
            let ident = &v.ident;
            let name_str = ident.to_string();

            if v.attrs.aliases.is_empty() {
                quote! {
                    if str_val == #name_str {
                        return toml_pretty_deser::TomlValue::new_ok(#name::#ident, helper.span());
                    }
                }
            } else {
                let aliases = &v.attrs.aliases;
                quote! {
                    if str_val == #name_str #(|| str_val == #aliases)* {
                        return toml_pretty_deser::TomlValue::new_ok(#name::#ident, helper.span());
                    }
                }
            }
        })
        .collect();

    // Collect all variant names + aliases for suggest_alternatives
    let all_names: Vec<String> = variant_infos
        .iter()
        .flat_map(|v| {
            let mut names = vec![v.ident.to_string()];
            names.extend(v.attrs.aliases.iter().cloned());
            names
        })
        .collect();

    let output = quote! {
        #original_item

        impl toml_pretty_deser::Visitor for #name {
            type Concrete = #name;

            fn fill_from_toml(helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<Self> {
                if let Some(str_val) = helper.item.as_str() {
                    #(#match_arms)*
                    return toml_pretty_deser::TomlValue::new_validation_failed(
                        helper.span(),
                        format!("Invalid enum variant: {}", str_val),
                        Some(toml_pretty_deser::suggest_alternatives(str_val, &[#(#all_names),*])),
                    );
                }
                toml_pretty_deser::TomlValue::new_wrong_type(helper.item, helper.span(), "string")
            }

            fn can_concrete(&self) -> bool {
                true
            }

            fn v_register_errors(&self, _col: &toml_pretty_deser::TomlCollector) {}

            fn into_concrete(self) -> Self::Concrete {
                self
            }
        }

        impl<__TpdR> toml_pretty_deser::VerifyVisitor<__TpdR> for #name {}
        impl<__TpdR> toml_pretty_deser::VerifyIn<__TpdR> for #name {}
    };

    output.into()
}

#[allow(clippy::too_many_lines)]
fn derive_tagged_enum(input: &DeriveInput, tag_key: &str) -> TokenStream {
    struct TaggedVariantInfo {
        ident: syn::Ident,
        inner_type: Type,
    }
    let original_item = emit_original_item(&input);
    let name = &input.ident;
    let partial_name = format_ident!("Partial{}", name);
    let vis = &input.vis;

    let variants = match &input.data {
        Data::Enum(data) => &data.variants,
        _ => unreachable!(),
    };


    let variant_infos: Vec<TaggedVariantInfo> = variants
        .iter()
        .map(|v| {
            let inner_type = match &v.fields {
                Fields::Unnamed(fields) => {
                    assert_eq!(
                        fields.unnamed.len(),
                        1,
                        "Tagged enum variants must have exactly one unnamed field"
                    );
                    fields.unnamed.first().expect("tagged enum variant has no fields").ty.clone()
                }
                _ => panic!(
                    "Tagged enum variants must have exactly one unnamed field: {}(InnerType)",
                    v.ident
                ),
            };
            TaggedVariantInfo {
                ident: v.ident.clone(),
                inner_type,
            }
        })
        .collect();

    // Generate PartialTaggedEnum variants: Variant(TomlValue<PartialInner>)
    let partial_variants: Vec<proc_macro2::TokenStream> = variant_infos
        .iter()
        .map(|v| {
            let ident = &v.ident;
            let inner_name = leaf_type_name(&v.inner_type);
            let partial_inner = format_ident!("Partial{}", inner_name);
            quote! { #ident(toml_pretty_deser::TomlValue<#partial_inner>) }
        })
        .collect();

    // Variant names for suggest_alternatives
    let variant_names: Vec<String> = variant_infos.iter().map(|v| v.ident.to_string()).collect();

    // fill_from_toml tag dispatch match arms
    let tag_match_arms: Vec<proc_macro2::TokenStream> = variant_infos
        .iter()
        .map(|v| {
            let ident = &v.ident;
            let ident_str = ident.to_string();
            let inner_name = leaf_type_name(&v.inner_type);
            let partial_inner = format_ident!("Partial{}", inner_name);

            quote! {
                #ident_str => {
                    let mut partial_inner = <#partial_inner as toml_pretty_deser::Visitor>::fill_from_toml(helper);
                    match &mut partial_inner.state {
                        toml_pretty_deser::TomlValueState::Ok { .. } => {
                            let visitor = #partial_name::#ident(partial_inner);
                            toml_pretty_deser::TomlValue::new_ok(visitor, helper.span())
                        }
                        toml_pretty_deser::TomlValueState::UnknownKeys(unknown_keys) => {
                            for k in unknown_keys.iter_mut() {
                                k.additional_spans.push((
                                    tag_span.clone(),
                                    "Involving this enum variant.".to_string(),
                                ));
                            }
                            let visitor = #partial_name::#ident(partial_inner);
                            toml_pretty_deser::TomlValue::new_nested(Some(visitor))
                        }
                        _ => {
                            let visitor = #partial_name::#ident(partial_inner);
                            toml_pretty_deser::TomlValue::new_nested(Some(visitor))
                        }
                    }
                }
            }
        })
        .collect();

    // can_concrete match arms
    let can_concrete_arms: Vec<proc_macro2::TokenStream> = variant_infos
        .iter()
        .map(|v| {
            let ident = &v.ident;
            quote! {
                #partial_name::#ident(toml_value) => toml_value.is_ok()
            }
        })
        .collect();

    // v_register_errors match arms
    let register_errors_arms: Vec<proc_macro2::TokenStream> = variant_infos
        .iter()
        .map(|v| {
            let ident = &v.ident;
            quote! {
                #partial_name::#ident(toml_value) => { toml_value.register_error(col); }
            }
        })
        .collect();

    // into_concrete match arms
    let into_concrete_arms: Vec<proc_macro2::TokenStream> = variant_infos
        .iter()
        .map(|v| {
            let ident = &v.ident;
            quote! {
                #partial_name::#ident(toml_value) => #name::#ident(toml_value.value.expect("into_concrete called when can_concrete returned false").into_concrete())
            }
        })
        .collect();

    // vv_validate match arms
    let vv_validate_arms: Vec<proc_macro2::TokenStream> = variant_infos
        .iter()
        .map(|v| {
            let ident = &v.ident;
            quote! {
                #partial_name::#ident(toml_value) => {
                    *toml_value = toml_value.take().tpd_validate(helper, parent);
                }
            }
        })
        .collect();

    let output = quote! {
        #original_item

        #[derive(Debug)]
        #vis enum #partial_name {
            #(#partial_variants,)*
        }

        impl toml_pretty_deser::Visitor for #partial_name {
            type Concrete = #name;

            fn fill_from_toml(helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<Self> {
                if !helper.is_table() {
                    return toml_pretty_deser::TomlValue::new_wrong_type(helper.item, helper.span(), "table or inline table");
                }
                let tag_value: toml_pretty_deser::TomlValue<String> = helper.get_with_aliases(#tag_key, &[]);
                if !tag_value.is_ok() {
                    return toml_pretty_deser::TomlValue {
                        value: None,
                        state: tag_value.state,
                    };
                }
                let tag = tag_value.value.as_ref().expect("tag value missing after is_ok check");
                let tag_span = tag_value.span();

                match tag.as_str() {
                    #(#tag_match_arms)*
                    _ => toml_pretty_deser::TomlValue::new_validation_failed(
                        tag_span,
                        format!("Invalid tag value: {}", tag),
                        Some(toml_pretty_deser::suggest_alternatives(tag, &[#(#variant_names),*])),
                    ),
                }
            }

            fn can_concrete(&self) -> bool {
                match self {
                    #(#can_concrete_arms,)*
                }
            }

            fn v_register_errors(&self, col: &toml_pretty_deser::TomlCollector) {
                match self {
                    #(#register_errors_arms)*
                }
            }

            fn into_concrete(self) -> #name {
                match self {
                    #(#into_concrete_arms,)*
                }
            }
        }

        impl<__TpdR> toml_pretty_deser::VerifyVisitor<__TpdR> for #partial_name {
            fn vv_validate(mut self, helper: &mut toml_pretty_deser::TomlHelper<'_>, parent: &__TpdR) -> Self
            where
                Self: Sized + toml_pretty_deser::Visitor,
            {
                match &mut self {
                    #(#vv_validate_arms)*
                }
                self
            }
        }

        impl<__TpdR> toml_pretty_deser::VerifyIn<__TpdR> for #partial_name {}
    };

    output.into()
}
