use std::{cell::RefCell, rc::Rc};

use toml_edit::Document;

use crate::case::FieldMatchMode;
use crate::collector::TomlSettings;
use crate::error::DeserError;
use crate::table_helper::TomlHelper;
use crate::traits::{TPDRoot, VerifyIn, VerifyVisitor, Visitor};
use crate::value::{TomlValueState, VecMode};

/// Helper to implement `T::tpd_from_toml` in toml-pretty-deser-macros
///
///
/// # Errors
///
/// On parsing & deserialization errors, returns `DeserError::ParsingFailure`
/// (with [`toml_edit::TomlError`]) or
/// `DeserError::DeserFailure` with the partially filled struct.
///
/// # Panics
///
/// When ok -> value present invariant is violated
pub fn deserialize_toml<P>(
    toml_str: &str,
    field_match_mode: FieldMatchMode,
    vec_mode: VecMode,
) -> Result<P::Concrete, DeserError<P>>
where
    P: Visitor + VerifyVisitor<TPDRoot> + VerifyIn<TPDRoot> + std::fmt::Debug + Default,
{
    let parsed_toml = toml_str
        .parse::<Document<String>>()
        .map_err(|toml_err| DeserError::ParsingFailure(toml_err, toml_str.to_string()))?;
    let _source = Rc::new(RefCell::new(toml_str.to_string()));

    let col = TomlSettings {
        match_mode: field_match_mode,
        vec_mode,
    };
    let top_level = parsed_toml.into_item();
    let mut helper = TomlHelper::from_item(&top_level, col.clone());

    let root = P::fill_from_toml(&mut helper);
    let mut root = root.tpd_validate(&TPDRoot);
    if helper.has_unknown() {
        root.state = TomlValueState::UnknownKeys(helper.unknown_spans());
    }

    if root.is_ok() {
        Ok(root.value.unwrap().into_concrete())
    } else {
        Err(DeserError::DeserFailure(toml_str.to_string(), Box::new(root)))
    }
}
