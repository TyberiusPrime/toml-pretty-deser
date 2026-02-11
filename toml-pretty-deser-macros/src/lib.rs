use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    Data, DeriveInput, Fields, GenericArgument, Lit, PathArguments, Type, TypePath,
    parse_macro_input,
};

/// Derive macro for generating TOML deserialization code.
///
/// # Struct-level attributes
/// - `#[tpd(root)]` - Top-level deserialization target (generates `TpdDeserializeStruct`)
/// - No `root` - Nested struct (generates `FromTomlTable` + `FromTomlItem`)
///
/// # Enum-level attributes
/// - `#[tpd(tag = "key")]` - Tagged enum with tag field name
/// - No `tag` - Simple string enum (generates `FromTomlItem`)
///
/// # Field-level attributes
/// - `#[tpd(nested)]` - Type has a Partial variant (nested struct or tagged enum)
/// - `#[tpd(alias("name1", "name2"))]` - Field aliases
/// - `#[tpd(skip)]` - Skip field, use `Default::default()` in `to_concrete`
/// - `#[tpd(default)]` - Use `Default::default()` if field is missing from TOML
/// - `#[tpd(with = "func")]` - Adapter function
/// - `#[tpd(with = "func", from = "Type")]` - Adapter function with type change
/// - `#[tpd(absorb_remaining)]` - Absorb all remaining unmatched fields
///
/// # Variant-level attributes (simple enum)
/// - `#[tpd(alias("name1", "name2"))]` - Variant aliases
#[proc_macro_derive(Tpd, attributes(tpd))]
pub fn derive_tpd(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match &input.data {
        Data::Struct(_) => derive_struct(input),
        Data::Enum(_) => derive_enum(input),
        Data::Union(_) => panic!("Tpd cannot be derived for unions"),
    }
}

// ============================================================================
// Attribute parsing
// ============================================================================

#[derive(Default)]
struct StructAttrs {
    is_root: bool,
}

fn parse_struct_attrs(input: &DeriveInput) -> StructAttrs {
    let mut attrs = StructAttrs::default();
    for attr in &input.attrs {
        if !attr.path().is_ident("tpd") {
            continue;
        }
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("root") {
                attrs.is_root = true;
            }
            Ok(())
        })
        .unwrap();
    }
    attrs
}

#[derive(Default)]
struct FieldAttrs {
    is_nested: bool,
    is_tagged: bool, // tagged enum field: like nested but no verify_struct
    aliases: Vec<String>,
    skip: bool,
    default: bool,
    with_fn: Option<String>,
    from_type: Option<String>,
    absorb_remaining: bool,
}

impl FieldAttrs {
    /// Whether the field's type has a Partial variant (nested struct or tagged enum)
    fn has_partial(&self) -> bool {
        self.is_nested || self.is_tagged
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
                attrs.is_nested = true;
            } else if meta.path.is_ident("tagged") {
                attrs.is_tagged = true;
            } else if meta.path.is_ident("skip") {
                attrs.skip = true;
            } else if meta.path.is_ident("default") {
                attrs.default = true;
            } else if meta.path.is_ident("absorb_remaining") {
                attrs.absorb_remaining = true;
            } else if meta.path.is_ident("with") {
                let value = meta.value().unwrap();
                let lit: Lit = value.parse().unwrap();
                if let Lit::Str(s) = lit {
                    attrs.with_fn = Some(s.value());
                }
            } else if meta.path.is_ident("from") {
                let value = meta.value().unwrap();
                let lit: Lit = value.parse().unwrap();
                if let Lit::Str(s) = lit {
                    attrs.from_type = Some(s.value());
                }
            } else if meta.path.is_ident("alias") {
                // #[tpd(alias("name1", "name2"))]
                let content;
                syn::parenthesized!(content in meta.input);
                let lits =
                    syn::punctuated::Punctuated::<Lit, syn::Token![,]>::parse_terminated(&content)
                        .unwrap();
                for lit in lits {
                    if let Lit::Str(s) = lit {
                        attrs.aliases.push(s.value());
                    }
                }
            }
            Ok(())
        })
        .unwrap();
    }
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
    Map(Type, Box<TypeKind>), // key type, value type
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

/// Get the leaf type name (last path segment without generics)
fn leaf_type_name(ty: &Type) -> String {
    if let Type::Path(TypePath { path, .. }) = ty {
        if let Some(last) = path.segments.last() {
            return last.ident.to_string();
        }
    }
    panic!("Cannot extract type name from {:?}", quote!(#ty).to_string());
}

/// Generate the partial type for a field's inner type (inside the outer TomlValue wrapper)
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

/// Check if a type contains a container (Vec/Map) inside it
fn has_container(kind: &TypeKind) -> bool {
    match kind {
        TypeKind::Leaf(_) => false,
        TypeKind::Optional(inner) | TypeKind::Boxed(inner) => has_container(inner),
        TypeKind::Vector(_) | TypeKind::Map(_, _) => true,
    }
}

/// Check if any conversion is needed inside this type (for Optional wrapping decisions)
fn needs_any_conversion(kind: &TypeKind, has_partial: bool) -> bool {
    match kind {
        TypeKind::Leaf(_) => has_partial,
        TypeKind::Optional(inner) | TypeKind::Boxed(inner) => {
            needs_any_conversion(inner, has_partial)
        }
        TypeKind::Vector(_) | TypeKind::Map(_, _) => true,
    }
}

/// Check if manual iteration is needed (vs simple .to_concrete()) for Vec/Map inner types
fn needs_manual_conversion(kind: &TypeKind, has_partial: bool) -> bool {
    match kind {
        TypeKind::Leaf(_) => has_partial,
        TypeKind::Optional(inner) | TypeKind::Boxed(inner) => {
            needs_manual_conversion(inner, has_partial)
        }
        TypeKind::Vector(inner) | TypeKind::Map(_, inner) => {
            has_container(inner) || needs_manual_conversion(inner, has_partial)
        }
    }
}

/// Generate the to_concrete expression for a field
fn gen_to_concrete_expr(
    field_ident: &syn::Ident,
    kind: &TypeKind,
    has_partial: bool,
    is_default: bool,
) -> proc_macro2::TokenStream {
    if is_default {
        return quote! {
            self.#field_ident.value.unwrap().unwrap_or_default()
        };
    }

    let base = quote! { self.#field_ident.value.unwrap() };
    gen_concrete_inner(base, kind, has_partial)
}

/// Generate the inner concrete conversion expression
fn gen_concrete_inner(
    expr: proc_macro2::TokenStream,
    kind: &TypeKind,
    has_partial: bool,
) -> proc_macro2::TokenStream {
    match kind {
        TypeKind::Leaf(_) => {
            if has_partial {
                quote! { #expr.to_concrete() }
            } else {
                expr
            }
        }
        TypeKind::Optional(inner) => {
            if needs_any_conversion(inner, has_partial) {
                let inner_conv = gen_concrete_inner(quote! { __v }, inner, has_partial);
                quote! { #expr.map(|__v| #inner_conv) }
            } else {
                expr
            }
        }
        TypeKind::Vector(inner) => {
            if needs_manual_conversion(inner, has_partial) || has_container(inner) {
                let inner_conv = gen_concrete_inner(
                    quote! { __v.value.unwrap() },
                    inner,
                    has_partial,
                );
                quote! {
                    #expr.into_iter().map(|__v| #inner_conv).collect()
                }
            } else {
                quote! { toml_pretty_deser::helpers::TpdDeserialize::to_concrete(#expr) }
            }
        }
        TypeKind::Map(_, inner) => {
            if needs_manual_conversion(inner, has_partial) || has_container(inner) {
                let inner_conv = gen_concrete_inner(
                    quote! { __v.value.unwrap() },
                    inner,
                    has_partial,
                );
                quote! {
                    #expr.into_iter().map(|(__k, __v)| (__k, #inner_conv)).collect()
                }
            } else {
                quote! { toml_pretty_deser::helpers::TpdDeserialize::to_concrete(#expr) }
            }
        }
        TypeKind::Boxed(inner) => gen_concrete_inner(expr, inner, has_partial),
    }
}

/// Generate the register_errors code for a field
fn gen_register_errors(
    field_ident: &syn::Ident,
    kind: &TypeKind,
    has_partial: bool,
) -> proc_macro2::TokenStream {
    let base_register = quote! {
        self.#field_ident.register_error(col);
    };

    // Descent into nested state for Vec, Map, nested structs, tagged enums
    // But NOT for Option-wrapped types (they don't have register_errors on Option<T>)
    enum DescentKind {
        None,
        TpdDeserialize, // Vec/Map: use TpdDeserialize::register_errors
        Inherent,       // Partial types: use inherent .register_errors()
    }

    let descent = match kind {
        TypeKind::Leaf(_) => {
            if has_partial {
                DescentKind::Inherent
            } else {
                DescentKind::None
            }
        }
        TypeKind::Vector(_) | TypeKind::Map(_, _) => DescentKind::TpdDeserialize,
        TypeKind::Optional(_) => DescentKind::None,
        TypeKind::Boxed(_) => {
            if has_partial {
                DescentKind::Inherent
            } else {
                DescentKind::None
            }
        }
    };

    match descent {
        DescentKind::None => base_register,
        DescentKind::TpdDeserialize => {
            quote! {
                #base_register
                if let toml_pretty_deser::TomlValueState::Nested = self.#field_ident.state {
                    if let Some(ref inner) = self.#field_ident.value {
                        toml_pretty_deser::helpers::TpdDeserialize::register_errors(inner, col);
                    }
                }
            }
        }
        DescentKind::Inherent => {
            quote! {
                #base_register
                if let toml_pretty_deser::TomlValueState::Nested = self.#field_ident.state {
                    if let Some(ref inner) = self.#field_ident.value {
                        inner.register_errors(col);
                    }
                }
            }
        }
    }
}

// ============================================================================
// Struct derivation
// ============================================================================

fn derive_struct(input: DeriveInput) -> TokenStream {
    let struct_attrs = parse_struct_attrs(&input);
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

    // Collect field info
    struct FieldInfo {
        ident: syn::Ident,
        attrs: FieldAttrs,
        kind: TypeKind,
        vis: syn::Visibility,
    }

    let field_infos: Vec<FieldInfo> = fields
        .iter()
        .map(|f| {
            let ident = f.ident.clone().unwrap();
            let attrs = parse_field_attrs(f);
            let kind = analyze_type(&f.ty);
            FieldInfo {
                ident,
                attrs,
                kind,
                vis: f.vis.clone(),
            }
        })
        .collect();

    // Generate partial struct fields (skip #[tpd(skip)] fields)
    let partial_fields: Vec<proc_macro2::TokenStream> = field_infos
        .iter()
        .filter(|f| !f.attrs.skip)
        .map(|f| {
            let ident = &f.ident;
            let fvis = &f.vis;
            if f.attrs.absorb_remaining {
                // absorb_remaining: same type transformation as normal IndexMap
                let inner_ty = gen_partial_inner_type(&f.kind, f.attrs.has_partial());
                quote! { #fvis #ident: toml_pretty_deser::TomlValue<#inner_ty> }
            } else if f.attrs.default {
                // default fields: wrap in Option
                let inner_ty = gen_partial_inner_type(&f.kind, f.attrs.has_partial());
                quote! { #fvis #ident: toml_pretty_deser::TomlValue<Option<#inner_ty>> }
            } else {
                let inner_ty = gen_partial_inner_type(&f.kind, f.attrs.has_partial());
                quote! { #fvis #ident: toml_pretty_deser::TomlValue<#inner_ty> }
            }
        })
        .collect();

    // Generate getter methods (skip absorb_remaining and skip fields)
    let getter_methods: Vec<proc_macro2::TokenStream> = field_infos
        .iter()
        .filter(|f| !f.attrs.skip && !f.attrs.absorb_remaining)
        .map(|f| {
            let ident = &f.ident;
            let getter_name = format_ident!("tpd_get_{}", ident);
            let field_name_str = ident.to_string();

            let aliases_expr = if f.attrs.aliases.is_empty() {
                quote! { &[] }
            } else {
                let aliases = &f.attrs.aliases;
                quote! { &[#(#aliases),*] }
            };

            // Determine the getter return type
            let is_option = matches!(&f.kind, TypeKind::Optional(_));

            if let Some(ref with_fn) = f.attrs.with_fn {
                let with_fn_ident: syn::Path = syn::parse_str(with_fn).unwrap();

                if let Some(ref from_type) = f.attrs.from_type {
                    // with + from: get as from_type, then apply adapter
                    let from_ty: Type = syn::parse_str(from_type).unwrap();
                    let inner_ty = gen_partial_inner_type(&f.kind, f.attrs.has_partial());
                    quote! {
                        fn #getter_name(&self, helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<#inner_ty> {
                            #with_fn_ident(helper.get_with_aliases::<#from_ty>(#field_name_str, #aliases_expr))
                        }
                    }
                } else {
                    // with only: get as field type, then apply adapter
                    let inner_ty = gen_partial_inner_type(&f.kind, f.attrs.has_partial());
                    quote! {
                        fn #getter_name(&self, helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<#inner_ty> {
                            #with_fn_ident(helper.get_with_aliases(#field_name_str, #aliases_expr))
                        }
                    }
                }
            } else if f.attrs.default {
                // default field: get as base type, then into_optional
                let inner_ty = gen_partial_inner_type(&f.kind, f.attrs.has_partial());
                quote! {
                    fn #getter_name(&self, helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<Option<#inner_ty>> {
                        helper.get_with_aliases(#field_name_str, #aliases_expr).into_optional()
                    }
                }
            } else if is_option {
                // Option<T> field: get inner, then into_optional
                let inner_ty = gen_partial_inner_type(&f.kind, f.attrs.has_partial());
                quote! {
                    fn #getter_name(&self, helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<#inner_ty> {
                        helper.get_with_aliases(#field_name_str, #aliases_expr).into_optional()
                    }
                }
            } else {
                let inner_ty = gen_partial_inner_type(&f.kind, f.attrs.has_partial());
                quote! {
                    fn #getter_name(&self, helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<#inner_ty> {
                        helper.get_with_aliases(#field_name_str, #aliases_expr)
                    }
                }
            }
        })
        .collect();

    // Generate to_concrete method
    let to_concrete_fields: Vec<proc_macro2::TokenStream> = field_infos
        .iter()
        .map(|f| {
            let ident = &f.ident;
            if f.attrs.skip {
                quote! { #ident: Default::default() }
            } else if f.attrs.absorb_remaining {
                let expr = gen_to_concrete_expr(ident, &f.kind, f.attrs.has_partial(), false);
                quote! { #ident: #expr }
            } else {
                let expr =
                    gen_to_concrete_expr(ident, &f.kind, f.attrs.has_partial(), f.attrs.default);
                quote! { #ident: #expr }
            }
        })
        .collect();

    // Generate can_concrete checks
    let can_concrete_checks: Vec<proc_macro2::TokenStream> = field_infos
        .iter()
        .filter(|f| !f.attrs.skip)
        .map(|f| {
            let ident = &f.ident;
            quote! { self.#ident.is_ok() }
        })
        .collect();

    // Generate register_errors calls
    let register_errors_calls: Vec<proc_macro2::TokenStream> = field_infos
        .iter()
        .filter(|f| !f.attrs.skip)
        .map(|f| {
            let ident = &f.ident;
            gen_register_errors(ident, &f.kind, f.attrs.has_partial())
        })
        .collect();

    if struct_attrs.is_root {
        // Root struct: generate TpdDeserializeStruct

        // Generate fill_fields body
        let fill_fields_stmts: Vec<proc_macro2::TokenStream> = field_infos
            .iter()
            .filter(|f| !f.attrs.skip)
            .map(|f| {
                let ident = &f.ident;
                let getter_name = format_ident!("tpd_get_{}", ident);

                if f.attrs.absorb_remaining {
                    quote! { self.#ident = helper.absorb_remaining(); }
                } else if f.attrs.is_nested && !matches!(&f.kind, TypeKind::Optional(_)) {
                    // Nested struct fields use verify_struct (not tagged enums)
                    // Only for direct nested types, not Vec<Nested> etc.
                    let is_direct_nested = matches!(&f.kind, TypeKind::Leaf(_));
                    if is_direct_nested {
                        quote! {
                            self.#ident = toml_pretty_deser::helpers::verify_struct(
                                self.#getter_name(helper), helper, self
                            );
                        }
                    } else {
                        quote! { self.#ident = self.#getter_name(helper); }
                    }
                } else {
                    quote! { self.#ident = self.#getter_name(helper); }
                }
            })
            .collect();

        // Reorder: absorb_remaining goes last
        let (regular_stmts, absorb_stmts): (Vec<_>, Vec<_>) = fill_fields_stmts
            .into_iter()
            .zip(field_infos.iter().filter(|f| !f.attrs.skip))
            .partition(|(_, f)| !f.attrs.absorb_remaining);

        let regular_stmts: Vec<_> = regular_stmts.into_iter().map(|(s, _)| s).collect();
        let absorb_stmts: Vec<_> = absorb_stmts.into_iter().map(|(s, _)| s).collect();

        let output = quote! {
            #[derive(Default, Debug)]
            #vis struct #partial_name {
                #(#partial_fields,)*
            }

            impl #partial_name {
                #(#getter_methods)*
            }

            impl toml_pretty_deser::helpers::TpdDeserializeStruct for #partial_name {
                type Concrete = #name;

                fn fill_fields(&mut self, helper: &mut toml_pretty_deser::TomlHelper<'_>) {
                    #(#regular_stmts)*
                    #(#absorb_stmts)*
                }

                fn can_concrete(&self) -> bool {
                    #(#can_concrete_checks)&&*
                }

                fn to_concrete(self) -> #name {
                    #name {
                        #(#to_concrete_fields,)*
                    }
                }

                fn register_errors(&self, col: &toml_pretty_deser::TomlCollector) {
                    #(#register_errors_calls)*
                }
            }
        };

        output.into()
    } else {
        // Nested struct: generate FromTomlTable + FromTomlItem

        // Generate from_toml_table body
        let from_toml_table_stmts: Vec<proc_macro2::TokenStream> = field_infos
            .iter()
            .filter(|f| !f.attrs.skip && !f.attrs.absorb_remaining)
            .map(|f| {
                let ident = &f.ident;
                let getter_name = format_ident!("tpd_get_{}", ident);

                let get_stmt = quote! { partial.#ident = partial.#getter_name(helper); };

                // For nested fields, add finalize_nested_field
                let is_direct_nested =
                    f.attrs.has_partial() && matches!(&f.kind, TypeKind::Leaf(_));
                if is_direct_nested {
                    quote! {
                        #get_stmt
                        toml_pretty_deser::helpers::finalize_nested_field(&mut partial.#ident, helper);
                    }
                } else {
                    get_stmt
                }
            })
            .collect();

        let output = quote! {
            #[derive(Default, Debug)]
            #vis struct #partial_name {
                #(#partial_fields,)*
            }

            impl #partial_name {
                #(#getter_methods)*

                #vis fn to_concrete(self) -> #name {
                    #name {
                        #(#to_concrete_fields,)*
                    }
                }

                #vis fn register_errors(&self, col: &toml_pretty_deser::TomlCollector) {
                    #(#register_errors_calls)*
                }
            }

            impl toml_pretty_deser::helpers::FromTomlTable for #partial_name {
                fn from_toml_table(helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<Self> {
                    let mut partial = Self::default();
                    #(#from_toml_table_stmts)*
                    toml_pretty_deser::TomlValue {
                        value: Some(partial),
                        state: toml_pretty_deser::TomlValueState::Nested,
                    }
                }

                fn can_concrete(&self) -> bool {
                    #(#can_concrete_checks)&&*
                }
            }

            impl toml_pretty_deser::FromTomlItem for #partial_name {
                fn from_toml_item(
                    item: &toml_edit::Item,
                    parent_span: std::ops::Range<usize>,
                    col: &toml_pretty_deser::TomlCollector,
                ) -> toml_pretty_deser::TomlValue<Self> {
                    toml_pretty_deser::helpers::from_toml_item_via_table(item, parent_span, col)
                }
            }
        };

        output.into()
    }
}

// ============================================================================
// Enum derivation
// ============================================================================

#[derive(Default)]
struct EnumAttrs {
    tag: Option<String>,
}

fn parse_enum_attrs(input: &DeriveInput) -> EnumAttrs {
    let mut attrs = EnumAttrs::default();
    for attr in &input.attrs {
        if !attr.path().is_ident("tpd") {
            continue;
        }
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("tag") {
                let value = meta.value().unwrap();
                let lit: Lit = value.parse().unwrap();
                if let Lit::Str(s) = lit {
                    attrs.tag = Some(s.value());
                }
            }
            Ok(())
        })
        .unwrap();
    }
    attrs
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
                        .unwrap();
                for lit in lits {
                    if let Lit::Str(s) = lit {
                        attrs.aliases.push(s.value());
                    }
                }
            }
            Ok(())
        })
        .unwrap();
    }
    attrs
}

fn derive_enum(input: DeriveInput) -> TokenStream {
    let enum_attrs = parse_enum_attrs(&input);

    if enum_attrs.tag.is_some() {
        derive_tagged_enum(input, enum_attrs)
    } else {
        derive_simple_enum(input)
    }
}

fn derive_simple_enum(input: DeriveInput) -> TokenStream {
    let name = &input.ident;

    let variants = match &input.data {
        Data::Enum(data) => &data.variants,
        _ => unreachable!(),
    };

    // Collect variant info
    struct VariantInfo {
        ident: syn::Ident,
        attrs: VariantAttrs,
    }

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
                        return toml_pretty_deser::TomlValue::new_ok(#name::#ident, parent_span);
                    }
                }
            } else {
                let aliases = &v.attrs.aliases;
                quote! {
                    if str_val == #name_str #(|| str_val == #aliases)* {
                        return toml_pretty_deser::TomlValue::new_ok(#name::#ident, parent_span);
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
        impl toml_pretty_deser::FromTomlItem for #name {
            fn from_toml_item(
                item: &toml_edit::Item,
                parent_span: std::ops::Range<usize>,
                _col: &toml_pretty_deser::TomlCollector,
            ) -> toml_pretty_deser::TomlValue<Self>
            where
                Self: Sized,
            {
                if let Some(str_val) = item.as_str() {
                    #(#match_arms)*
                    toml_pretty_deser::TomlValue::new_validation_failed(
                        item.span().unwrap_or(parent_span.clone()),
                        "Invalid value.".to_string(),
                        Some(toml_pretty_deser::suggest_alternatives(str_val, &[#(#all_names),*])),
                    )
                } else {
                    toml_pretty_deser::TomlValue::new_wrong_type(item, parent_span, "string")
                }
            }
        }
    };

    output.into()
}

fn derive_tagged_enum(input: DeriveInput, enum_attrs: EnumAttrs) -> TokenStream {
    let name = &input.ident;
    let partial_name = format_ident!("Partial{}", name);
    let tag_key = enum_attrs.tag.unwrap();
    let vis = &input.vis;

    let variants = match &input.data {
        Data::Enum(data) => &data.variants,
        _ => unreachable!(),
    };

    // Collect variant info
    struct TaggedVariantInfo {
        ident: syn::Ident,
        inner_type: Type,
    }

    let variant_infos: Vec<TaggedVariantInfo> = variants
        .iter()
        .map(|v| {
            let inner_type = match &v.fields {
                Fields::Unnamed(fields) => {
                    assert_eq!(fields.unnamed.len(), 1, "Tagged enum variants must have exactly one unnamed field");
                    fields.unnamed.first().unwrap().ty.clone()
                }
                _ => panic!("Tagged enum variants must have exactly one unnamed field: {}(InnerType)", v.ident),
            };
            TaggedVariantInfo {
                ident: v.ident.clone(),
                inner_type,
            }
        })
        .collect();

    // Generate PartialTaggedEnum variants
    let partial_variants: Vec<proc_macro2::TokenStream> = variant_infos
        .iter()
        .map(|v| {
            let ident = &v.ident;
            let inner_name = leaf_type_name(&v.inner_type);
            let partial_inner = format_ident!("Partial{}", inner_name);
            quote! { #ident(#partial_inner) }
        })
        .collect();

    // Generate to_concrete match arms
    let to_concrete_arms: Vec<proc_macro2::TokenStream> = variant_infos
        .iter()
        .map(|v| {
            let ident = &v.ident;
            quote! {
                Self::#ident(inner) => #name::#ident(inner.to_concrete())
            }
        })
        .collect();

    // Generate register_errors match arms
    let register_errors_arms: Vec<proc_macro2::TokenStream> = variant_infos
        .iter()
        .map(|v| {
            let ident = &v.ident;
            quote! {
                Self::#ident(inner) => { inner.register_errors(col); }
            }
        })
        .collect();

    // Generate FromTomlItem match arms for tag dispatch
    let variant_names: Vec<String> = variant_infos.iter().map(|v| v.ident.to_string()).collect();

    let tag_match_arms: Vec<proc_macro2::TokenStream> = variant_infos
        .iter()
        .map(|v| {
            let ident = &v.ident;
            let ident_str = ident.to_string();
            let inner_name = leaf_type_name(&v.inner_type);
            let partial_inner = format_ident!("Partial{}", inner_name);

            quote! {
                #ident_str => {
                    let partial_inner = <#partial_inner as toml_pretty_deser::helpers::FromTomlTable>::from_toml_table(&mut tag_helper);
                    if let Some(inner) = partial_inner.value {
                        if inner.can_concrete() && tag_helper.no_unknown() {
                            toml_pretty_deser::TomlValue::new_ok(#partial_name::#ident(inner), parent_span)
                        } else {
                            tag_helper.register_unknown();
                            toml_pretty_deser::TomlValue {
                                value: Some(#partial_name::#ident(inner)),
                                state: toml_pretty_deser::TomlValueState::Nested,
                            }
                        }
                    } else {
                        partial_inner.convert_failed_type()
                    }
                }
            }
        })
        .collect();

    let output = quote! {
        #[derive(Debug)]
        #vis enum #partial_name {
            #(#partial_variants,)*
        }

        impl #partial_name {
            #vis fn to_concrete(self) -> #name {
                match self {
                    #(#to_concrete_arms,)*
                }
            }

            #vis fn register_errors(&self, col: &toml_pretty_deser::TomlCollector) {
                match self {
                    #(#register_errors_arms)*
                }
            }
        }

        impl toml_pretty_deser::FromTomlItem for #partial_name {
            fn from_toml_item(
                item: &toml_edit::Item,
                parent_span: std::ops::Range<usize>,
                col: &toml_pretty_deser::TomlCollector,
            ) -> toml_pretty_deser::TomlValue<Self> {
                if let Some(table) = <toml_edit::Item as toml_pretty_deser::AsTableLikePlus>::as_table_like_plus(item) {
                    let mut tag_helper = toml_pretty_deser::TomlHelper::from_table(table, col.clone());
                    let tag_value: toml_pretty_deser::TomlValue<String> = tag_helper.get_with_aliases(#tag_key, &[]);

                    if !tag_value.is_ok() {
                        return toml_pretty_deser::TomlValue {
                            value: None,
                            state: tag_value.state,
                        };
                    }

                    let tag = tag_value.value.as_ref().unwrap();
                    let tag_span = tag_value.span();

                    match tag.as_str() {
                        #(#tag_match_arms)*
                        _ => toml_pretty_deser::TomlValue::new_validation_failed(
                            tag_span,
                            format!("Invalid tag value: {}", tag),
                            Some(toml_pretty_deser::suggest_alternatives(tag, &[#(#variant_names),*])),
                        ),
                    }
                } else {
                    toml_pretty_deser::TomlValue::new_wrong_type(item, parent_span, "table or inline table")
                }
            }
        }
    };

    output.into()
}
