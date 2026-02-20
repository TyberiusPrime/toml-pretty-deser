use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::parse::Parser as _;
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
/// - `#[tpd(alias = "name1", alias = "name2")]` - Field aliases
/// - `#[tpd(skip)]` - Skip field, use `Default::default()` in `into_concrete`
/// - `#[tpd(default)]` - Use `Default::default()` if field is missing from TOML
/// - `#[tpd(with = "func")]` - Adapter function
/// - `#[tpd(absorb_remaining)]` - Absorb all remaining unmatched fields
///
/// # Variant-level attributes (simple enum)
/// - `#[tpd(alias = "name1", alias = "name2")]` - Variant aliases
///
/// # Compile-time errors
/// Unknown or incompatible attributes are reported as compiler errors pointing at the
/// offending token, not as unhelpful panics during macro expansion.
#[proc_macro_attribute]
pub fn tpd(attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    let attr_ts: TokenStream2 = attr.into();
    let result = match &input.data {
        Data::Struct(_) => derive_struct(&input, attr_ts),
        Data::Enum(_) => derive_enum(&input, attr_ts),
        Data::Union(_) => Err(syn::Error::new_spanned(
            &input.ident,
            "tpd cannot be applied to unions",
        )),
    };
    result.unwrap_or_else(|e| e.to_compile_error()).into()
}

// ============================================================================
// Attribute parsing
// ============================================================================

struct MacroAttrs {
    root: bool,
    no_verify: bool,
}

fn parse_macro_attrs(attr_ts: TokenStream2) -> syn::Result<MacroAttrs> {
    let mut attrs = MacroAttrs {
        root: false,
        no_verify: false,
    };
    syn::meta::parser(|meta| {
        if meta.path.is_ident("root") {
            attrs.root = true;
            Ok(())
        } else if meta.path.is_ident("no_verify") {
            attrs.no_verify = true;
            Ok(())
        } else {
            Err(meta.error(format!(
                "unknown tpd struct attribute `{}`",
                meta.path
                    .get_ident()
                    .map_or_else(|| "?".to_string(), ToString::to_string),
            )))
        }
    })
    .parse2(attr_ts)?;
    Ok(attrs)
}

fn parse_enum_attrs(attr_ts: TokenStream2) -> syn::Result<Option<(String, Vec<String>)>> {
    let mut tag: Option<String> = None;
    let mut aliases: Vec<String> = Vec::new();
    syn::meta::parser(|meta| {
        if meta.path.is_ident("tag") {
            let value = meta.value()?;
            let lit: Lit = value.parse()?;
            if let Lit::Str(s) = lit {
                tag = Some(s.value());
            }
            Ok(())
        } else if meta.path.is_ident("alias") {
            let value = meta.value()?;
            let lit: Lit = value.parse()?;
            if let Lit::Str(s) = lit {
                aliases.push(s.value());
            }
            Ok(())
        } else {
            Err(meta.error(format!(
                "unknown tpd enum attribute `{}`",
                meta.path
                    .get_ident()
                    .map_or_else(|| "?".to_string(), ToString::to_string),
            )))
        }
    })
    .parse2(attr_ts)?;
    Ok(tag.map(|t| (t, aliases)))
}

/// Strip #[tpd(...)] attributes from a `DeriveInput` and return the cleaned item as tokens
fn emit_original_item(input: &DeriveInput) -> TokenStream2 {
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

enum AdaptInVerify {
    /// `adapt_in_verify(SomeType)` — user explicitly provides the intermediate type (leaf/non-nested use)
    Explicit(TokenStream2),
    /// `adapt_in_verify` with no type arg, combined with `#[tpd(nested)]` — macro auto-detects
    /// the partial type from the field's inner generic argument.
    Auto,
}

#[derive(Default)]
struct FieldAttrs {
    nested: NestedState,
    aliases: Vec<String>,
    skip: bool,
    default: bool,
    with_fn: Option<String>,
    absorb_remaining: bool,
    adapt_in_verify: Option<AdaptInVerify>,
}

impl FieldAttrs {
    fn has_partial(&self) -> bool {
        matches!(self.nested, NestedState::Nested | NestedState::Tagged)
    }
}

fn parse_field_attrs(field: &syn::Field) -> syn::Result<FieldAttrs> {
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
                let value = meta.value()?;
                let lit: Lit = value.parse()?;
                if let Lit::Str(s) = lit {
                    attrs.with_fn = Some(s.value());
                }
            } else if meta.path.is_ident("alias") {
                let value = meta.value()?;
                let lit: Lit = value.parse()?;
                if let Lit::Str(s) = lit {
                    attrs.aliases.push(s.value());
                }
            } else if meta.path.is_ident("adapt_in_verify") {
                if meta.input.peek(syn::token::Paren) {
                    let content;
                    syn::parenthesized!(content in meta.input);
                    let ty: syn::Type = content.parse()?;
                    attrs.adapt_in_verify = Some(AdaptInVerify::Explicit(quote! { #ty }));
                } else {
                    attrs.adapt_in_verify = Some(AdaptInVerify::Auto);
                }
            } else {
                return Err(meta.error(format!(
                    "unknown tpd field attribute `{}`",
                    meta.path
                        .get_ident()
                        .map_or_else(|| "?".to_string(), ToString::to_string),
                )));
            }
            Ok(())
        })?;
    }

    //yeah that's a pile of worms, because the adapted values 
    //are not necessarily Visitor.
    if attrs.has_partial() && attrs.with_fn.is_some() {
    return Err(syn::Error::new_spanned(
        &field.ty,
        "field cannot have both #[tpd(nested)] and #[tpd(with = \"...\")]",
    ));
    }

    if let Some(AdaptInVerify::Explicit(_)) = &attrs.adapt_in_verify {
        if matches!(attrs.nested, NestedState::Nested | NestedState::Tagged) {
            return Err(syn::Error::new_spanned(
                &field.ty,
                "adapt_in_verify(Type) cannot be combined with nested/tagged; \
                 use adapt_in_verify (no type arg) with nested instead",
            ));
        }
    }
    if attrs.adapt_in_verify.is_some()
        && (attrs.with_fn.is_some() || attrs.skip || attrs.absorb_remaining || attrs.default)
    {
        return Err(syn::Error::new_spanned(
            &field.ty,
            "adapt_in_verify cannot be combined with with/skip/absorb_remaining/default",
        ));
    }

    Ok(attrs)
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
    /// `Box<T>`: auto-detected; generates `Box<PartialT>` in partial (Box impl Visitor).
    Boxed(Box<TypeKind>),
}

fn analyze_type(ty: &Type) -> syn::Result<TypeKind> {
    if let Type::Path(TypePath { path, .. }) = ty {
        if let Some(last) = path.segments.last() {
            let name = last.ident.to_string();
            match name.as_str() {
                "Option" => {
                    let inner = extract_single_generic(&last.arguments, ty)?;
                    Ok(TypeKind::Optional(Box::new(analyze_type(&inner)?)))
                }
                "Vec" => {
                    let inner = extract_single_generic(&last.arguments, ty)?;
                    Ok(TypeKind::Vector(Box::new(analyze_type(&inner)?)))
                }
                "IndexMap" => {
                    let (key, value) = extract_two_generics(&last.arguments, ty)?;
                    Ok(TypeKind::Map(key, Box::new(analyze_type(&value)?)))
                }
                "Box" => {
                    let inner = extract_single_generic(&last.arguments, ty)?;
                    Ok(TypeKind::Boxed(Box::new(analyze_type(&inner)?)))
                }
                "HashMap" | "BTreeMap" => Err(syn::Error::new_spanned(
                    ty,
                    format!(
                        "`{name}` is not supported as a tpd map field type; \
                         use `IndexMap<K, V>` from the `indexmap` crate instead"
                    ),
                )),
                _ => Ok(TypeKind::Leaf(ty.clone())),
            }
        } else {
            Ok(TypeKind::Leaf(ty.clone()))
        }
    } else {
        Ok(TypeKind::Leaf(ty.clone()))
    }
}

fn extract_single_generic(args: &PathArguments, context_ty: &Type) -> syn::Result<Type> {
    if let PathArguments::AngleBracketed(ab) = args
        && let Some(GenericArgument::Type(ty)) = ab.args.first()
    {
        return Ok(ty.clone());
    }
    Err(syn::Error::new_spanned(
        context_ty,
        "expected a single generic type argument here",
    ))
}

fn extract_two_generics(args: &PathArguments, context_ty: &Type) -> syn::Result<(Type, Type)> {
    if let PathArguments::AngleBracketed(ab) = args {
        let mut iter = ab.args.iter();
        if let (Some(GenericArgument::Type(k)), Some(GenericArgument::Type(v))) =
            (iter.next(), iter.next())
        {
            return Ok((k.clone(), v.clone()));
        }
    }
    Err(syn::Error::new_spanned(
        context_ty,
        "expected two generic type arguments here",
    ))
}

/// Recursively extracts the innermost concrete type from nested generic wrappers.
/// E.g. `std::sync::Arc<Outer>` → `Outer`, `Rc<RefCell<Outer>>` → `Outer`.
/// Stops when a type with no generic arguments is found (that's the nested struct type).
/// Used by the `adapt_in_verify` (Auto) case to find the partial type.
fn extract_innermost_type(ty: &Type) -> syn::Result<Type> {
    if let Type::Path(TypePath { path, .. }) = ty {
        if let Some(last) = path.segments.last() {
            if let PathArguments::AngleBracketed(ab) = &last.arguments {
                if ab.args.len() == 1 {
                    if let GenericArgument::Type(inner) = &ab.args[0] {
                        return extract_innermost_type(inner);
                    }
                } else if ab.args.len() > 1 {
                    return Err(syn::Error::new_spanned(
                        ty,
                        "adapt_in_verify (without type argument) cannot auto-detect the nested \
                         type when the wrapper has multiple generic arguments",
                    ));
                }
            }
            // No generic args (or non-angle-bracketed): this is the innermost type
            return Ok(ty.clone());
        }
    }
    Err(syn::Error::new_spanned(
        ty,
        "adapt_in_verify (without type argument) requires the field type to be a path type",
    ))
}

/// Given a type like `foo::bar::MyStruct`, return a token stream for `foo::bar::PartialMyStruct`.
/// For a simple `MyStruct`, returns `PartialMyStruct`.
/// For wrapper types like `Box<Inner>`, returns `Box<PartialInner>`.
fn partial_type_path(ty: &Type) -> syn::Result<TokenStream2> {
    if let Type::Path(TypePath { qself: None, path }) = ty {
        let mut segments = path.segments.clone();
        if let Some(last) = segments.last_mut() {
            // Check if this is a wrapper type (Box, etc.) with a single generic argument
            if last.ident == "Box" {
                if let syn::PathArguments::AngleBracketed(args) = &last.arguments {
                    if args.args.len() == 1 {
                        if let syn::GenericArgument::Type(inner_ty) = &args.args[0] {
                            let partial_inner = partial_type_path(inner_ty)?;
                            let leading_colon = &path.leading_colon;
                            let prefix_segments = segments.iter().take(segments.len() - 1);
                            return Ok(
                                quote! { #leading_colon #(#prefix_segments ::)* Box<#partial_inner> },
                            );
                        }
                    }
                }
            }
            last.ident = format_ident!("Partial{}", last.ident);
        }
        let leading_colon = &path.leading_colon;
        return Ok(quote! { #leading_colon #segments });
    }
    Err(syn::Error::new_spanned(
        ty,
        "cannot generate a Partial type for this type; tpd nested types must be simple type paths",
    ))
}

/// Generate the partial type for a field's inner type (inside the outer `TomlValue` wrapper)
fn gen_partial_inner_type(kind: &TypeKind, is_nested: bool) -> syn::Result<TokenStream2> {
    match kind {
        TypeKind::Leaf(ty) => {
            if is_nested {
                // Use partial_type_path to correctly handle module-qualified types
                // e.g. `mod::Inner` → `mod::PartialInner`
                partial_type_path(ty)
            } else {
                Ok(quote! { #ty })
            }
        }
        TypeKind::Optional(inner) => {
            let inner_ty = gen_partial_inner_type(inner, is_nested)?;
            Ok(quote! { Option<#inner_ty> })
        }
        TypeKind::Vector(inner) => {
            let inner_ty = gen_partial_inner_type(inner, is_nested)?;
            Ok(quote! { Vec<toml_pretty_deser::TomlValue<#inner_ty>> })
        }
        TypeKind::Map(key, inner) => {
            let inner_ty = gen_partial_inner_type(inner, is_nested)?;
            Ok(quote! { toml_pretty_deser::MapAndKeys<#key, #inner_ty> })
        }
        TypeKind::Boxed(inner) => {
            let inner_ty = gen_partial_inner_type(inner, is_nested)?;
            Ok(quote! { Box<#inner_ty> })
        }
    }
}

/// Check if a type is the unit type `()`
fn is_unit_type(kind: &TypeKind) -> bool {
    if let TypeKind::Leaf(ty) = kind
        && let Type::Tuple(tuple) = ty
    {
        return tuple.elems.is_empty();
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
fn derive_struct(input: &DeriveInput, attr_ts: TokenStream2) -> syn::Result<TokenStream2> {
    struct FieldInfo {
        ident: syn::Ident,
        attrs: FieldAttrs,
        kind: TypeKind,
        vis: syn::Visibility,
        ty: Type,
    }

    let original_item = emit_original_item(input);
    let macro_attrs = parse_macro_attrs(attr_ts)?;
    let is_root = macro_attrs.root;
    let no_verify = macro_attrs.no_verify;
    let name = &input.ident;
    let partial_name = format_ident!("Partial{}", name);
    let vis = &input.vis;

    let fields = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(named) => &named.named,
            _ => {
                return Err(syn::Error::new_spanned(
                    input.ident.clone(),
                    "tpd only supports named struct fields",
                ));
            }
        },
        _ => unreachable!(),
    };

    let field_infos: Vec<FieldInfo> = fields
        .iter()
        .map(|f| -> syn::Result<FieldInfo> {
            let ident = f.ident.clone().ok_or_else(|| {
                syn::Error::new_spanned(&f.ty, "tpd only supports named struct fields")
            })?;
            let attrs = parse_field_attrs(f)?;
            // Skipped fields are emitted verbatim as `Option<FieldType>`; their kind is never
            // used for code generation, so skip the type analysis (which would reject HashMap etc.)
            let kind = if attrs.skip {
                TypeKind::Leaf(f.ty.clone())
            } else {
                analyze_type(&f.ty)?
            };

            if attrs.absorb_remaining && !is_indexmap_type(&f.ty) {
                return Err(syn::Error::new_spanned(
                    &f.ty,
                    "#[tpd(absorb_remaining)] requires the field type to be `IndexMap<K, V>`",
                ));
            }

            Ok(FieldInfo {
                ident,
                attrs,
                kind,
                vis: f.vis.clone(),
                ty: f.ty.clone(),
            })
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let absorb_fields: Vec<&FieldInfo> = field_infos
        .iter()
        .filter(|f| f.attrs.absorb_remaining)
        .collect();
    if absorb_fields.len() > 1 {
        return Err(syn::Error::new(
            absorb_fields[1].ident.span(),
            "only one field can have #[tpd(absorb_remaining)]",
        ));
    }

    // --- Partial struct fields ---
    let partial_fields: Vec<TokenStream2> = field_infos
        .iter()
        .map(|f| -> syn::Result<TokenStream2> {
            let ident = &f.ident;
            let fvis = &f.vis;
            let ftype = &f.ty;
            if f.attrs.skip {
                Ok(quote! { #fvis #ident: Option<#ftype> })
            } else if let Some(AdaptInVerify::Explicit(ref intermediate_ty)) = f.attrs.adapt_in_verify {
                Ok(quote! { #fvis #ident: toml_pretty_deser::TomlValue<toml_pretty_deser::MustAdapt<#intermediate_ty, #ftype>> })
            } else if matches!(f.attrs.adapt_in_verify, Some(AdaptInVerify::Auto)) && f.attrs.has_partial() {
                // nested + adapt_in_verify (no type arg): auto-detect partial from inner generic type
                let inner_ty = extract_innermost_type(ftype)?;
                let partial_inner_ty = partial_type_path(&inner_ty)?;
                Ok(quote! { #fvis #ident: toml_pretty_deser::TomlValue<toml_pretty_deser::MustAdaptNested<#partial_inner_ty, #ftype>> })
            } else if matches!(f.attrs.adapt_in_verify, Some(AdaptInVerify::Auto)) {
                // adapt_in_verify (no type arg) without nested: default to toml_edit::Item
                Ok(quote! { #fvis #ident: toml_pretty_deser::TomlValue<toml_pretty_deser::MustAdapt<toml_edit::Item, #ftype>> })
            } else {
                let inner_ty = gen_partial_inner_type(&f.kind, f.attrs.has_partial())?;
                Ok(quote! { #fvis #ident: toml_pretty_deser::TomlValue<#inner_ty> })
            }
        })
        .collect::<syn::Result<Vec<_>>>()?;

    // --- Getter methods ---
    let getter_methods: Vec<TokenStream2> = field_infos
        .iter()
        .filter(|f| !f.attrs.skip && !f.attrs.absorb_remaining)
        .map(|f| -> syn::Result<TokenStream2> {
            let ident = &f.ident;
            let getter_name = format_ident!("tpd_get_{}", ident);
            let field_name_str = ident.to_string();
            let inner_ty = gen_partial_inner_type(&f.kind, f.attrs.has_partial())?;

            // Unit type () fields: no TOML lookup
            if is_unit_type(&f.kind) {
                return Ok(quote! {
                    fn #getter_name(&self, _helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<()> {
                        toml_pretty_deser::TomlValue::new_ok((), 0..0)
                    }
                });
            }

            let aliases_expr = if f.attrs.aliases.is_empty() {
                quote! { &[] }
            } else {
                let aliases = &f.attrs.aliases;
                quote! { &[#(#aliases),*] }
            };

            let is_option = matches!(&f.kind, TypeKind::Optional(_));
            let is_vec = matches!(&f.kind, TypeKind::Vector(_));
            let is_map = matches!(&f.kind, TypeKind::Map(_, _));

            if let Some(AdaptInVerify::Explicit(ref intermediate_ty)) = f.attrs.adapt_in_verify {
                let concrete_ty = &f.ty;
                return Ok(quote! {
                    fn #getter_name(&self, helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<toml_pretty_deser::MustAdapt<#intermediate_ty, #concrete_ty>> {
                        helper.get_with_aliases(#field_name_str, #aliases_expr)
                    }
                });
            }
            if matches!(f.attrs.adapt_in_verify, Some(AdaptInVerify::Auto)) {
                let concrete_ty = &f.ty;
                if f.attrs.has_partial() {
                    // nested + adapt_in_verify (no type arg): MustAdaptNested with auto partial
                    let inner_ty = extract_innermost_type(concrete_ty)?;
                    let partial_inner_ty = partial_type_path(&inner_ty)?;
                    return Ok(quote! {
                        fn #getter_name(&self, helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<toml_pretty_deser::MustAdaptNested<#partial_inner_ty, #concrete_ty>> {
                            helper.get_with_aliases(#field_name_str, #aliases_expr)
                        }
                    });
                } else {
                    // adapt_in_verify (no type arg) without nested: toml_edit::Item
                    return Ok(quote! {
                        fn #getter_name(&self, helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<toml_pretty_deser::MustAdapt<toml_edit::Item, #concrete_ty>> {
                            helper.get_with_aliases(#field_name_str, #aliases_expr)
                        }
                    });
                }
            }

            if let Some(ref with_fn) = f.attrs.with_fn {
                let with_fn_ident: syn::Path = syn::parse_str(with_fn).map_err(|_| {
                    syn::Error::new(
                        ident.span(),
                        format!("#[tpd(with)] value `{with_fn}` is not a valid Rust path"),
                    )
                })?;
                if is_option {
                    // For Option<T> fields: the with-func sees T (not Option<T>).
                    // We call into_optional() on the result so the field stays Option<T>.
                    Ok(quote! {
                        fn #getter_name(&self, helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<#inner_ty> {
                            #with_fn_ident(helper.get_with_aliases(#field_name_str, #aliases_expr)).into_optional()
                        }
                    })
                } else if is_vec {
                    // For Vec<T> fields: the with-func sees T (not Vec<T>).
                    // get_vec_with_adapter anchors the source element type via the fn signature.
                    Ok(quote! {
                        fn #getter_name(&self, helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<#inner_ty> {
                            helper.get_vec_with_adapter(#field_name_str, #aliases_expr, #with_fn_ident)
                        }
                    })
                } else if is_map {
                    // For IndexMap<K, V> fields: the with-func sees V (not the whole map).
                    // get_map_with_adapter anchors the source value type via the fn signature.
                    Ok(quote! {
                        fn #getter_name(&self, helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<#inner_ty> {
                            helper.get_map_with_adapter(#field_name_str, #aliases_expr, #with_fn_ident)
                        }
                    })
                } else {
                    Ok(quote! {
                        fn #getter_name(&self, helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<#inner_ty> {
                            #with_fn_ident(helper.get_with_aliases(#field_name_str, #aliases_expr))
                        }
                    })
                }
            } else if f.attrs.default {
                Ok(quote! {
                    fn #getter_name(&self, helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<#inner_ty> {
                        let mut t = helper.get_with_aliases(#field_name_str, #aliases_expr);
                        toml_pretty_deser::TomlOr::or_default(&mut t);
                        t
                    }
                })
            } else if is_option {
                Ok(quote! {
                    fn #getter_name(&self, helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<#inner_ty> {
                        helper.get_with_aliases(#field_name_str, #aliases_expr).into_optional()
                    }
                })
            } else {
                Ok(quote! {
                    fn #getter_name(&self, helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<#inner_ty> {
                        helper.get_with_aliases(#field_name_str, #aliases_expr)
                    }
                })
            }
        })
        .collect::<syn::Result<Vec<_>>>()?;

    // --- fill_from_toml statements ---
    let fill_stmts: Vec<(TokenStream2, bool)> = field_infos
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

    let regular_fill_stmts: Vec<&TokenStream2> = fill_stmts
        .iter()
        .filter(|(_, is_absorb)| !is_absorb)
        .map(|(s, _)| s)
        .collect();
    let absorb_fill_stmts: Vec<&TokenStream2> = fill_stmts
        .iter()
        .filter(|(_, is_absorb)| *is_absorb)
        .map(|(s, _)| s)
        .collect();

    // --- can_concrete checks ---
    let can_concrete_checks: Vec<TokenStream2> = field_infos
        .iter()
        .filter(|f| !f.attrs.skip)
        .map(|f| {
            let ident = &f.ident;
            quote! { self.#ident.is_ok() }
        })
        .collect();

    // --- v_register_errors ---
    let register_errors_calls: Vec<TokenStream2> = field_infos
        .iter()
        .filter(|f| !f.attrs.skip)
        .map(|f| {
            let ident = &f.ident;
            // `with` fields hold an already-adapted leaf: use the no-Visitor-bound variant
            if f.attrs.with_fn.is_some() {
                quote! { self.#ident.register_error_leaf(col); }
            } else {
                quote! { self.#ident.register_error(col); }
            }
        })
        .collect();

    // --- into_concrete fields ---
    let into_concrete_fields: Vec<TokenStream2> = field_infos
        .iter()
        .map(|f| {
            let ident = &f.ident;
            if f.attrs.skip && f.attrs.default {
                quote! { #ident: self.#ident.unwrap_or_default() }
            }
            else if f.attrs.skip {
                quote! { #ident: self.#ident.expect("Expected #ident to have been set in VerifyIn") }
            } else if is_unit_type(&f.kind) {
                quote! { #ident: () }
            } else if f.attrs.adapt_in_verify.is_some() {
                quote! { #ident: self.#ident.value.expect("into concrete when can_concrete returned false").into_concrete() }
            } else if f.attrs.with_fn.is_some() && matches!(&f.kind, TypeKind::Vector(_)) {
                // with + Vec: inner values are already the concrete type (adapter produced them),
                // so just unwrap each TomlValue without calling into_concrete (which requires Visitor).
                quote! { #ident: self.#ident.value.expect("into concrete when can_concrete returned false")
                    .into_iter().map(|v| v.value.expect("inner value missing after with adapter")).collect() }
            } else if f.attrs.with_fn.is_some() && matches!(&f.kind, TypeKind::Map(_, _)) {
                // with + Map: inner values are already the concrete type, just unwrap.
                quote! { #ident: self.#ident.value.expect("into concrete when can_concrete returned false")
                    .map.into_iter().map(|(k, v)| (k, v.value.expect("inner value missing after with adapter"))).collect() }
            } else if needs_into_concrete(&f.kind, f.attrs.has_partial()) {
                quote! { #ident: self.#ident.value.expect("into concrete when can_concrete returned false").into_concrete() }
            } else {
                quote! { #ident: self.#ident.value.expect("into concrete when can_concrete returned false") }
            }
        })
        .collect();

    // --- vv_validate statements ---
    let vv_validate_stmts: Vec<TokenStream2> = field_infos
        .iter()
        // `with` fields are already in their final form after fill_from_toml; no tpd_validate needed
        .filter(|f| !f.attrs.skip && !is_unit_type(&f.kind) && f.attrs.with_fn.is_none())
        .map(|f| {
            let ident = &f.ident;
            quote! { self.#ident = self.#ident.take().tpd_validate(&self); }
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
                    toml_pretty_deser::deserialize_toml::<#partial_name>(toml_str, field_match_mode, vec_mode)
                }
            }
        }
    } else {
        quote! {}
    };

    Ok(quote! {
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
            fn vv_validate(mut self, _parent: &__TpdR) -> Self
            where
                Self: Sized + toml_pretty_deser::Visitor,
            {
                #(#vv_validate_stmts)*
                self
            }
        }

        #no_verify_impl

        #root_impl
    })
}

// ============================================================================
// Enum derivation
// ============================================================================

struct VariantAttrs {
    aliases: Vec<String>,
    skip: bool,
}

fn parse_variant_attrs(variant: &syn::Variant) -> syn::Result<VariantAttrs> {
    let mut attrs = VariantAttrs {
        aliases: Vec::new(),
        skip: false,
    };
    for attr in &variant.attrs {
        if !attr.path().is_ident("tpd") {
            continue;
        }
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("alias") {
                let value = meta.value()?;
                let lit: Lit = value.parse()?;
                if let Lit::Str(s) = lit {
                    attrs.aliases.push(s.value());
                }
            } else if meta.path.is_ident("skip") {
                attrs.skip = true;
            } else {
                return Err(meta.error(format!(
                    "unknown tpd variant attribute `{}`",
                    meta.path
                        .get_ident()
                        .map_or_else(|| "?".to_string(), ToString::to_string),
                )));
            }
            Ok(())
        })?;
    }
    Ok(attrs)
}

fn derive_enum(input: &DeriveInput, attr_ts: TokenStream2) -> syn::Result<TokenStream2> {
    let tag = parse_enum_attrs(attr_ts)?;

    if let Some((tag, aliases)) = tag {
        derive_tagged_enum(input, &tag, &aliases)
    } else {
        derive_simple_enum(input)
    }
}

fn derive_simple_enum(input: &DeriveInput) -> syn::Result<TokenStream2> {
    struct VariantInfo {
        ident: syn::Ident,
        attrs: VariantAttrs,
    }
    let original_item = emit_original_item(input);
    let name = &input.ident;

    let variants = match &input.data {
        Data::Enum(data) => &data.variants,
        _ => unreachable!(),
    };

    let variant_infos: Vec<VariantInfo> = variants
        .iter()
        .map(|v| -> syn::Result<VariantInfo> {
            if !v.fields.is_empty() {
                return Err(syn::Error::new_spanned(
                    v,
                    "simple enums must have unit variants; \
                     use #[tpd(tag = \"key\")] for tagged enums",
                ));
            }
            Ok(VariantInfo {
                ident: v.ident.clone(),
                attrs: parse_variant_attrs(v)?,
            })
        })
        .collect::<syn::Result<Vec<_>>>()?;

    // Generate match arms for string matching
    let match_arms: Vec<TokenStream2> = variant_infos
        .iter()
        .filter(|v| !v.attrs.skip)
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
        .filter(|v| !v.attrs.skip)
        .flat_map(|v| {
            let mut names = vec![v.ident.to_string()];
            names.extend(
                v.attrs
                    .aliases
                    .iter()
                    .map(|alias| format!("'{alias}' (='{ident}')", ident = v.ident)),
            );
            names
        })
        .collect();

    Ok(quote! {
        #original_item

        impl toml_pretty_deser::Visitor for #name {
            type Concrete = #name;

            fn fill_from_toml(helper: &mut toml_pretty_deser::TomlHelper<'_>) -> toml_pretty_deser::TomlValue<Self> {
                if let Some(str_val) = helper.item.as_str() {
                    #(#match_arms)*
                    return toml_pretty_deser::TomlValue::new_validation_failed(
                        helper.span(),
                        format!("Invalid enum variant: '{}'", str_val),
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
    })
}

#[allow(clippy::too_many_lines)]
fn derive_tagged_enum(
    input: &DeriveInput,
    tag_key: &str,
    tag_aliases: &[String],
) -> syn::Result<TokenStream2> {
    struct TaggedVariantInfo {
        ident: syn::Ident,
        inner_type: Type,
        attrs: VariantAttrs,
    }
    let original_item = emit_original_item(input);
    let name = &input.ident;
    let partial_name = format_ident!("Partial{}", name);
    let vis = &input.vis;

    let variants = match &input.data {
        Data::Enum(data) => &data.variants,
        _ => unreachable!(),
    };

    let variant_infos: Vec<TaggedVariantInfo> = variants
        .iter()
        .map(|v| -> syn::Result<TaggedVariantInfo> {
            let inner_type = match &v.fields {
                Fields::Unnamed(fields) => {
                    if fields.unnamed.len() != 1 {
                        return Err(syn::Error::new_spanned(
                            v,
                            "tagged enum variants must have exactly one unnamed field",
                        ));
                    }
                    fields.unnamed.first().expect("checked above").ty.clone()
                }
                _ => {
                    return Err(syn::Error::new_spanned(
                        v,
                        format!(
                            "tagged enum variant `{}` must have exactly one unnamed field: \
                             {}(InnerType)",
                            v.ident, v.ident
                        ),
                    ));
                }
            };
            Ok(TaggedVariantInfo {
                ident: v.ident.clone(),
                inner_type,
                attrs: parse_variant_attrs(v)?,
            })
        })
        .collect::<syn::Result<Vec<_>>>()?;

    // Generate PartialTaggedEnum variants: Variant(TomlValue<PartialInner>)
    let partial_variants: Vec<TokenStream2> = variant_infos
        .iter()
        .filter(|v| !v.attrs.skip)
        .map(|v| -> syn::Result<TokenStream2> {
            let ident = &v.ident;
            let partial_inner = partial_type_path(&v.inner_type)?;
            Ok(quote! { #ident(toml_pretty_deser::TomlValue<#partial_inner>) })
        })
        .collect::<syn::Result<Vec<_>>>()?;

    // Variant names + aliases for suggest_alternatives (skipped variants excluded)
    let variant_names: Vec<String> = variant_infos
        .iter()
        .filter(|v| !v.attrs.skip)
        .flat_map(|v| {
            let mut names = vec![v.ident.to_string()];
            names.extend(
                v.attrs
                    .aliases
                    .iter()
                    .map(|alias| format!("'{alias}' (='{ident}')", ident = v.ident)),
            );
            names
        })
        .collect();

    // fill_from_toml tag dispatch match arms
    let tag_match_arms: Vec<TokenStream2> = variant_infos
        .iter()
        .filter(|v| !v.attrs.skip)
        .map(|v| -> syn::Result<TokenStream2> {
            let ident = &v.ident;
            let ident_str = ident.to_string();
            let partial_inner = partial_type_path(&v.inner_type)?;
            let aliases = &v.attrs.aliases;

            Ok(quote! {
                #ident_str #(| #aliases)* => {
                    let mut partial_inner = <#partial_inner as toml_pretty_deser::Visitor>::fill_from_toml(helper);
                    match &mut partial_inner.state {
                        toml_pretty_deser::TomlValueState::Ok => {
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
            })
        })
        .collect::<syn::Result<Vec<_>>>()?;

    // can_concrete match arms
    let can_concrete_arms: Vec<TokenStream2> = variant_infos
        .iter()
        .filter(|v| !v.attrs.skip)
        .map(|v| {
            let ident = &v.ident;
            quote! {
                #partial_name::#ident(toml_value) => toml_value.is_ok()
            }
        })
        .collect();

    // v_register_errors match arms
    let register_errors_arms: Vec<TokenStream2> = variant_infos
        .iter()
        .filter(|v| !v.attrs.skip)
        .map(|v| {
            let ident = &v.ident;
            quote! {
                #partial_name::#ident(toml_value) => { toml_value.register_error(col); }
            }
        })
        .collect();

    // into_concrete match arms
    let into_concrete_arms: Vec<TokenStream2> = variant_infos
        .iter()
        .filter(|v| !v.attrs.skip)
        .map(|v| {
            let ident = &v.ident;
            quote! {
                #partial_name::#ident(toml_value) => #name::#ident(toml_value.value.expect("into_concrete called when can_concrete returned false").into_concrete())
            }
        })
        .collect();

    // vv_validate match arms
    let vv_validate_arms: Vec<TokenStream2> = variant_infos
        .iter()
        .filter(|v| !v.attrs.skip)
        .map(|v| {
            let ident = &v.ident;
            quote! {
                #partial_name::#ident(toml_value) => {
                    *toml_value = toml_value.take().tpd_validate(parent);
                }
            }
        })
        .collect();

    Ok(quote! {
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
                let tag_value: toml_pretty_deser::TomlValue<String> = helper.get_with_aliases(#tag_key, &[#(#tag_aliases),*]);
                if !tag_value.is_ok() {
                    return toml_pretty_deser::TomlValue {
                        value: None,
                        state: tag_value.state,
                        span: tag_value.span,
                        help: tag_value.help,
                    };
                }
                let tag = tag_value.value.as_ref().expect("tag value missing after is_ok check");
                let tag_span = tag_value.span();

                match tag.as_str() {
                    #(#tag_match_arms)*
                    _ => toml_pretty_deser::TomlValue::new_validation_failed(
                        tag_span,
                        format!("Invalid tag value: '{}'", tag),
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
            fn vv_validate(mut self, parent: &__TpdR) -> Self
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
    })
}
