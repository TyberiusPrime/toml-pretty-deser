use std::{cell::RefCell, rc::Rc};

use indexmap::IndexMap;
use toml_edit::Document;

use crate::{
    AsTableLikePlus, DeserError, FieldMatchMode, TomlCollector, TomlHelper, TomlValue,
    TomlValueState, VecMode, VerifyTomlItem,
};

pub trait FromTomlTable: Sized {
    fn from_toml_table(helper: &mut TomlHelper<'_>) -> TomlValue<Self>;
    fn can_concrete(&self) -> bool;
}

pub fn from_toml_item_via_table<T: FromTomlTable>(
    item: &toml_edit::Item,
    parent_span: std::ops::Range<usize>,
    col: &TomlCollector,
) -> TomlValue<T> {
    if let Some(table) = item.as_table_like_plus() {
        let mut helper = TomlHelper::from_table(table, col.clone());
        let mut result = T::from_toml_table(&mut helper);

        if let Some(ref inner) = result.value {
            if inner.can_concrete() {
                if helper.no_unknown() {
                    result.state = TomlValueState::Ok {
                        span: item.span().unwrap_or(parent_span),
                    };
                } else {
                    helper.register_unknown();
                }
            }
        }

        result
    } else {
        TomlValue::new_wrong_type(item, parent_span, "table or inline table")
    }
}

pub fn verify_struct<T: VerifyTomlItem<R> + FromTomlTable + std::fmt::Debug, R>(
    temp: TomlValue<T>,
    helper: &mut TomlHelper<'_>,
    parent: &R,
) -> TomlValue<T> {
    if temp.value.is_some() {
        let span = temp.span();
        let res = temp.value.unwrap().verify_struct(helper, parent); //that's the user implemented verify.
        if !res.can_concrete() {
            TomlValue {
                value: Some(res),
                state: TomlValueState::Nested,
            }
        } else {
            TomlValue::new_ok(res, span)
        }
    } else {
        temp
    }
}

#[macro_export]
macro_rules! impl_from_toml_item_for_table {
    ($($ty:ty),+ $(,)?) => {
        $(
            impl FromTomlItem for $ty {
                fn from_toml_item(
                    item: &toml_edit::Item,
                    parent_span: std::ops::Range<usize>,
                    col: &TomlCollector,
                ) -> TomlValue<Self> {
                    from_toml_item_via_table(item, parent_span, col)
                }
            }
        )+
    };
}

pub fn finalize_nested_field<T: FromTomlTable>(
    field: &mut TomlValue<T>,
    helper: &mut TomlHelper<'_>,
) {
    if let Some(inner) = field.value.as_ref() {
        if inner.can_concrete() {
            if helper.no_unknown() {
                field.state = TomlValueState::Ok { span: field.span() };
            } else {
                helper.register_unknown();
            }
        }
    }
}

//what the #[tdp] marked PartialT structs implement
pub trait TpdDeserializeStruct: Default {
    type Concrete;
    fn fill_fields(&mut self, helper: &mut TomlHelper<'_>);
    fn can_concrete(&self) -> bool;

    /// # Panics
    /// When can_concrete() return false and it's nevertheless called
    fn to_concrete(self) -> Self::Concrete;
    fn register_errors(&self, col: &TomlCollector);
}

// what our internal Vec,IndexMap, etc implement
pub trait TpdDeserialize: Default {
    type Concrete;
    //no can_concrete, rely on state: TomlValueState::Ok
    fn to_concrete(self) -> Self::Concrete;
    fn register_errors(&self, col: &TomlCollector);
}

impl<T> TpdDeserialize for Vec<TomlValue<T>> {
    type Concrete = Vec<T>;

    fn to_concrete(self) -> Self::Concrete {
        self.into_iter().map(|item| item.value.unwrap()).collect()
    }

    fn register_errors(&self, col: &TomlCollector) {
        for val in self.iter() {
            val.register_error(col);
        }
    }
}

impl<T, S: std::hash::Hash + Eq> TpdDeserialize for IndexMap<S, TomlValue<T>> {
    type Concrete = IndexMap<S, T>;

    fn to_concrete(self) -> Self::Concrete {
        self.into_iter()
            .map(|(k, v)| (k, v.value.unwrap()))
            .collect()
    }

    fn register_errors(&self, col: &TomlCollector) {
        for val in self.values() {
            val.register_error(col);
        }
    }
}

pub fn deserialize_toml<P: TpdDeserializeStruct + VerifyTomlItem<()>>(
    toml_str: &str,
    field_match_mode: FieldMatchMode,
    vec_mode: VecMode,
) -> Result<P::Concrete, DeserError<P>> {
    let parsed_toml = toml_str
        .parse::<Document<String>>()
        .map_err(|toml_err| DeserError::ParsingFailure(toml_err, toml_str.to_string()))?;
    let source = Rc::new(RefCell::new(toml_str.to_string()));

    let col = TomlCollector {
        errors: Rc::new(RefCell::new(Vec::new())),
        match_mode: field_match_mode,
        vec_mode,
        context_spans: Rc::new(RefCell::new(Vec::new())),
    };
    let mut helper = TomlHelper::from_table(parsed_toml.as_table(), col.clone());

    let mut partial = P::default();
    partial.fill_fields(&mut helper);
    let partial = partial.verify_struct(&mut helper, &());

    if helper.no_unknown() && partial.can_concrete() {
        Ok(partial.to_concrete())
    } else {
        helper.register_unknown();
        partial.register_errors(&col);
        Err(DeserError::DeserFailure(
            helper.into_inner(&source),
            partial,
        ))
    }
}
