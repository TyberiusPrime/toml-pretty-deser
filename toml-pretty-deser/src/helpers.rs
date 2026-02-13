use std::{cell::RefCell, rc::Rc};

use indexmap::IndexMap;
use toml_edit::Document;

use crate::{
    AnnotatedError, AnnotatedErrorExt, AsTableLikePlus, DeserError, FieldMatchMode, SpannedMessage,
    TomlCollector, TomlHelper, TomlValue, TomlValueState, VecMode,
};

pub trait Visitor: Sized {
    type Concrete;

    /// Populate self from TOML. Called by TomlValue<Self>::fill_from_toml,
    /// which handles the Missing/TypeError envelope before delegating here.
    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self>;

    /// Macro-derived: recursively checks all TomlValue<_> fields are .is_ok()
    fn can_concrete(&self) -> bool;

    fn register_errors(&self, col: &TomlCollector);

    /// Consume into concrete. Allowed to panic if !can_concrete().
    fn into_concrete(self) -> Self::Concrete;
}

pub trait VerifyVisitor<Parent> {
    #[allow(unused_variables)]
    fn vv_validate(self, helper: &mut TomlHelper<'_>, parent: &Parent) -> Option<Self>
    where
        Self: Sized + Visitor,
    {
        if self.can_concrete() {
            Some(self)
        } else {
            None
        }
    }
}

#[derive(Default)]
pub struct Root;

impl<T> TomlValue<T>
where
    T: Visitor,
{
    pub fn tpd_verify<R>(self, helper: &mut TomlHelper, parent: &R) -> TomlValue<T>
    where
        T: Visitor + VerifyVisitor<R>,
    {
        match self.state {
            TomlValueState::Ok { .. } => {
                let span = self.span();
                if let Some(validated_value) = self.value.unwrap().vv_validate(helper, parent) {
                    TomlValue::new_ok(validated_value, span)
                } else {
                    TomlValue::new_validation_failed(span, "Validation failed".to_string(), None)
                }
            }
            _ => self,
        }
    }

    /// Register an error using the context spans from the collector.
    pub fn register_error(&self, col: &TomlCollector) {
        let context = col.get_context_spans();
        self.register_error_with_context(col, &context);
    }
    /// Register an error with additional context spans that will be appended to the error.
    pub fn register_error_with_context(
        &self,
        col: &TomlCollector,
        context_spans: &[SpannedMessage],
    ) {
        let mut err = match &self.state {
            TomlValueState::NotSet | TomlValueState::Ok { .. } => {
                return;
            } //ignored, we expect the errors below to have been added
            TomlValueState::Nested => {
                self.value.as_ref().unwrap().register_errors(col);
                return;
            }
            TomlValueState::Missing { key, parent_span } => AnnotatedError::placed(
                parent_span.clone(),
                &format!("Missing required key: '{key}'."),
                "This key is required but was not found in the TOML document.",
            ),
            TomlValueState::MultiDefined { key, spans } => {
                let mut err = AnnotatedError::placed(
                    spans[0].clone(),
                    "Key/alias conflict (defined multiple times)",
                    &format!("Use only one of the keys involved. Canonical is '{key}'"),
                );
                for span in spans.iter().skip(1) {
                    err.add_span(span.clone(), "Also defined here");
                }
                err
            }
            TomlValueState::WrongType {
                span,
                expected,
                found,
            } => AnnotatedError::placed(
                span.clone(),
                &format!("Wrong type: expected {expected}, found {found}"),
                "This value has the wrong type.",
            ),
            TomlValueState::ValidationFailed {
                span,
                message,
                help,
            } => AnnotatedError::placed(
                span.clone(),
                message,
                help.as_ref().map_or("", std::string::String::as_str),
            ),
        };

        // Add context spans to the error
        for context in context_spans {
            err.add_span(context.span.clone(), &context.msg);
        }

        col.errors.borrow_mut().push(err);
    }
}

pub fn deserialize_toml<P>(
    toml_str: &str,
    field_match_mode: FieldMatchMode,
    vec_mode: VecMode,
) -> Result<P::Concrete, DeserError<P>>
where
    P: Visitor + VerifyVisitor<Root>+std::fmt::Debug,
{
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
    let top_level = parsed_toml.into_item();
    let mut helper = TomlHelper::from_item(&top_level, col.clone());

    let root = P::fill_from_toml(&mut helper);
    let root = root.tpd_verify(&mut helper, &Root);

    dbg!(&root);
    if helper.no_unknown() && root.is_ok() {
        Ok(root.value.unwrap().into_concrete())
    } else {
        helper.register_unknown();
        root.register_error(&col);
        Err(DeserError::DeserFailure(
            helper.into_inner(&source),
            root.value.unwrap(),
        ))
    }
}

impl<R> VerifyVisitor<R> for u8 {}
impl<R, T: VerifyVisitor<R>> VerifyVisitor<R> for Option<T> {}

impl<R, T: Visitor + VerifyVisitor<R>> VerifyVisitor<R> for Vec<TomlValue<T>> {
    fn vv_validate(self, helper: &mut TomlHelper<'_>, parent: &R) -> Option<Self>
    where
        Self: Sized + Visitor,
    {
        let v: Vec<TomlValue<T>> = self
            .into_iter()
            .map(|entry| entry.tpd_verify(helper, parent))
            .collect();
        if v.iter().all(|item| item.is_ok()) {
            Some(v)
        } else {
            None
        }
    }
}

impl<K: std::hash::Hash + Eq, R, T: Visitor + VerifyVisitor<R>> VerifyVisitor<R>
    for IndexMap<K, TomlValue<T>>
{
    fn vv_validate(self, helper: &mut TomlHelper<'_>, parent: &R) -> Option<Self>
    where
        Self: Sized + Visitor,
    {
        let out: IndexMap<K, TomlValue<T>> = self
            .into_iter()
            .map(|(k, v)| {
                let validated_value = v.tpd_verify(helper, parent);
                (k, validated_value)
            })
            .collect();
        if out.values().all(|v| v.is_ok()) {
            Some(out)
        } else {
            None
        }
    }
}

impl Visitor for u8 {
    type Concrete = u8;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        match helper.item.as_integer() {
            Some(v) => match TryInto::<u8>::try_into(v) {
                Ok(v) => {
                    return TomlValue::new_ok(v, helper.span());
                }
                Err(_) => {
                    return TomlValue::new_wrong_type(
                        &helper.item,
                        helper.span(),
                        "integer (u8), range exceeded",
                    );
                }
            },
            None => {
                return TomlValue::new_wrong_type(&helper.item, helper.span(), "integer (u8)");
            }
        }
    }

    fn can_concrete(&self) -> bool {
        true
    }

    fn register_errors(&self, _col: &TomlCollector) {}

    fn into_concrete(self) -> Self::Concrete {
        self
    }
}

impl<R> VerifyVisitor<R> for String {}

impl Visitor for String {
    type Concrete = String;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        match helper.item.as_str() {
            Some(v) => TomlValue::new_ok(v.to_string(), helper.span()),
            None => TomlValue::new_wrong_type(&helper.item, helper.span(), "integer (u8)"),
        }
    }

    fn can_concrete(&self) -> bool {
        true
    }

    fn register_errors(&self, _col: &TomlCollector) {}

    fn into_concrete(self) -> Self::Concrete {
        self
    }
}

impl<T: Visitor> Visitor for Vec<TomlValue<T>> {
    type Concrete = Vec<T::Concrete>;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        match helper.item {
            toml_edit::Item::ArrayOfTables(_array_of_tables) => todo!(),
            toml_edit::Item::Value(toml_edit::Value::Array(array)) => {
                let res: Vec<TomlValue<T>> = array
                    .iter()
                    .map(|entry| {
                        T::fill_from_toml(&mut TomlHelper::from_item(
                            &toml_edit::Item::Value(entry.clone()), //todo: can we do this without
                            //clone
                            helper.col.clone(),
                        ))
                    })
                    .collect();
                if res.iter().all(|item| item.is_ok()) {
                    TomlValue::new_ok(res, helper.span())
                } else {
                    TomlValue::new_nested(Some(res))
                }
            }
            _ => TomlValue::new_wrong_type(&helper.item, helper.span(), "array"),
        }
    }

    fn can_concrete(&self) -> bool {
        self.iter().all(|item| item.is_ok())
    }

    fn register_errors(&self, _col: &TomlCollector) {}

    fn into_concrete(self) -> Self::Concrete {
        self.into_iter()
            .map(|item| item.value.unwrap().into_concrete())
            .collect()
    }
}

impl<T, K: From<String> + std::hash::Hash + Eq> Visitor for IndexMap<K, TomlValue<T>>
where
    T: Visitor,
{
    type Concrete = IndexMap<K, T::Concrete>;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        match helper.item.as_table_like_plus() {
            Some(table) => {
                let mut result = IndexMap::new();
                let mut all_ok = true;
                for (key, value) in table.iter() {
                    let key: K = key.to_string().into();
                    let mut value_helper = TomlHelper::from_item(value, helper.col.clone());
                    let deserialized_value = T::fill_from_toml(&mut value_helper);
                    if !deserialized_value.is_ok() {
                        all_ok = false;
                    }
                    result.insert(key, deserialized_value);
                }
                if all_ok {
                    TomlValue::new_ok(result, helper.span())
                } else {
                    TomlValue::new_nested(Some(result))
                }
            }
            None => TomlValue::new_wrong_type(&helper.item, helper.span(), "table"),
        }
    }

    fn can_concrete(&self) -> bool {
        self.values().all(|v| v.is_ok())
    }

    fn register_errors(&self, _col: &TomlCollector) {}

    fn into_concrete(self) -> Self::Concrete {
        self.into_iter()
            .map(|(k, v)| (k, v.value.unwrap().into_concrete()))
            .collect()
    }
}

impl<T: Visitor> Visitor for Option<T> {
    type Concrete = Option<T>;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        //Optional etc is being handled upstream by get_with_alias / tpd_get_* ..into_optional()
        let p = T::fill_from_toml(helper);
        if p.is_ok() {
            TomlValue::new_ok(Some(p.value.unwrap()), helper.span())
        } else {
            p.convert_failed_type()
        }
    }

    fn can_concrete(&self) -> bool {
        match self {
            Some(v) => v.can_concrete(),
            None => true,
        }
    }

    fn register_errors(&self, col: &TomlCollector) {
        match self {
            Some(v) => v.register_errors(col),
            None => {}
        }
    }

    fn into_concrete(self) -> Self::Concrete {
        self
    }
}
