use std::{cell::RefCell, rc::Rc};

use indexmap::IndexMap;
use toml_edit::Document;

use crate::{
    AnnotatedError, AnnotatedErrorExt, AsTableLikePlus, DeserError, FieldMatchMode, SpannedMessage,
    TomlCollector, TomlHelper, TomlValue, TomlValueState, VecMode,
};

pub trait Visitor: Sized + std::fmt::Debug {
    type Concrete;

    /// Populate self from TOML. Called by TomlValue<Self>::fill_from_toml,
    /// which handles the Missing/TypeError envelope before delegating here.
    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self>;

    /// Macro-derived: recursively checks all TomlValue<_> fields are .is_ok()
    fn can_concrete(&self) -> bool;

    fn v_register_errors(&self, col: &TomlCollector);

    /// Consume into concrete. Allowed to panic if !can_concrete().
    fn into_concrete(self) -> Self::Concrete;
}

pub trait VerifyVisitor<Parent> {
    #[allow(unused_variables)]
    fn vv_validate(self, helper: &mut TomlHelper<'_>, parent: &Parent) -> Self
    where
        Self: Sized + Visitor,
    {
        self
    }
}

pub trait VerifyIn<Parent> {
    #[allow(unused_variables)]
    fn verify(
        &mut self,
        helper: &mut TomlHelper<'_>,
        parent: &Parent,
    ) -> Result<(), (String, Option<String>)>
    where
        Self: Sized + Visitor,
    {
        Ok(())
    }
}

#[derive(Default)]
pub struct Root;

impl<T> TomlValue<T>
where
    T: Visitor,
{
    pub fn from_visitor(visitor: T, helper: &TomlHelper<'_>) -> Self {
        if helper.has_unknown() {
            TomlValue {
                value: Some(visitor),
                state: TomlValueState::UnknownKeys {
                    spans: helper.unknown_spans(),
                },
            }
        } else if visitor.can_concrete() {
            TomlValue::new_ok(visitor, helper.span())
        } else {
            TomlValue::new_nested(Some(visitor))
        }
    }

    pub fn tpd_validate<R>(self, helper: &mut TomlHelper, parent: &R) -> TomlValue<T>
    where
        T: Visitor + VerifyVisitor<R> + VerifyIn<R>,
    {
        match self.state {
            TomlValueState::Ok { .. } => {
                let span = self.span();
                let mut maybe_validated = self.value.unwrap().vv_validate(helper, parent);
                match maybe_validated.verify(helper, parent) {
                    Ok(()) => TomlValue::new_ok(maybe_validated, span),

                    Err((msg, hint)) => TomlValue::new_validation_failed(span, msg, hint),
                }
            }
            TomlValueState::Nested => {
                if let Some(value) = self.value {
                    let mut maybe_validated = value.vv_validate(helper, parent);
                    maybe_validated.verify(helper, parent).ok();
                    if maybe_validated.can_concrete() {
                        TomlValue::new_ok(maybe_validated, helper.span())
                    } else {
                        TomlValue::new_nested(Some(maybe_validated))
                    }
                    // {
                    //     Ok(()) => TomlValue::new_nested(Some(maybe_validated)),
                    //
                    //     Err((msg, hint)) => TomlValue::new_validation_failed(helper.span(), msg, hint),
                    // }
                } else {
                    self
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
        let errs: Vec<AnnotatedError> = match &self.state {
            TomlValueState::NotSet | TomlValueState::Ok { .. } => {
                return;
            } //ignored, we expect the errors below to have been added
            TomlValueState::Nested => {
                if let Some(value) = self.value.as_ref() {
                    value.v_register_errors(col);
                }
                return;
            }
            TomlValueState::Missing { key, parent_span } => vec![AnnotatedError::placed(
                parent_span.clone(),
                &format!("Missing required key: '{key}'."),
                "",
            )],
            TomlValueState::MultiDefined { key, spans } => {
                let mut err = AnnotatedError::placed(
                    spans[0].clone(),
                    "Key/alias conflict (defined multiple times)",
                    &format!("Use only one of the keys involved. Canonical is '{key}'"),
                );
                for span in spans.iter().skip(1) {
                    err.add_span(span.clone(), "Also defined here");
                }
                vec![err]
            }
            TomlValueState::WrongType {
                span,
                expected,
                found,
            } => vec![AnnotatedError::placed(
                span.clone(),
                &format!("Wrong type: expected {expected}, found {found}"),
                "This value has the wrong type.",
            )],
            TomlValueState::ValidationFailed {
                span,
                message,
                help,
            } => vec![AnnotatedError::placed(
                span.clone(),
                message,
                help.as_ref().map_or("", std::string::String::as_str),
            )],
            TomlValueState::UnknownKeys { spans } => {
                if let Some(value) = self.value.as_ref() {
                    value.v_register_errors(col);
                };
                spans
                    .iter()
                    .map(|(_key, span, help)| {
                        AnnotatedError::placed(span.clone(), "Unknown key.", help)
                    })
                    .collect()
            }
        };

        for mut err in errs.into_iter() {
            // Add context spans to the error
            for context in context_spans {
                err.add_span(context.span.clone(), &context.msg);
            }
            col.errors.borrow_mut().push(err);
        }
    }
}

pub fn deserialize_toml<P>(
    toml_str: &str,
    field_match_mode: FieldMatchMode,
    vec_mode: VecMode,
) -> Result<P::Concrete, DeserError<P>>
where
    P: Visitor + VerifyVisitor<Root> + VerifyIn<Root> + std::fmt::Debug,
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
    dbg!(&root);
    let mut root = root.tpd_validate(&mut helper, &Root);
    dbg!(&root);
    if helper.has_unknown() {
        root.state = TomlValueState::UnknownKeys {
            spans: helper.unknown_spans(),
        }
    }

    if root.is_ok() {
        Ok(root.value.unwrap().into_concrete())
    } else {
        root.register_error(&col);
        Err(DeserError::DeserFailure(
            helper.into_inner(&source),
            root.value.unwrap(),
        ))
    }
}

impl<R> VerifyVisitor<R> for u8 {}
impl<R> VerifyIn<R> for u8 {}

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
                return TomlValue::new_wrong_type(&helper.item, helper.span(), "u8");
            }
        }
    }

    fn can_concrete(&self) -> bool {
        true
    }

    fn v_register_errors(&self, _col: &TomlCollector) {}

    fn into_concrete(self) -> Self::Concrete {
        self
    }
}

impl<R> VerifyVisitor<R> for String {}
impl<R> VerifyIn<R> for String {}

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

    fn v_register_errors(&self, _col: &TomlCollector) {}

    fn into_concrete(self) -> Self::Concrete {
        self
    }
}

impl<T: Visitor> Visitor for Option<T> {
    type Concrete = Option<T::Concrete>;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        //Optional etc is being handled upstream by get_with_alias / tpd_get_* ..into_optional()
        let p = T::fill_from_toml(helper);
        if p.is_ok() {
            TomlValue::new_ok(Some(p.value.unwrap()), helper.span())
        } else {
            TomlValue {
                value: Some(p.value),
                state: p.state,
            }
        }
    }

    fn can_concrete(&self) -> bool {
        match self {
            Some(v) => v.can_concrete(),
            None => true,
        }
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        match self {
            Some(v) => v.v_register_errors(col),
            None => {}
        }
    }

    fn into_concrete(self) -> Option<T::Concrete> {
        self.map(|t| t.into_concrete())
    }
}

impl<R, T: Visitor + VerifyVisitor<R>> VerifyVisitor<R> for Option<T> {
    fn vv_validate(self, helper: &mut TomlHelper<'_>, parent: &R) -> Self {
        match self {
            Some(v) => Some(v.vv_validate(helper, parent)),
            None => None,
        }
    }
}

impl<R, T: VerifyIn<R>> VerifyIn<R> for Option<T> {}

impl<T: Visitor> Visitor for Vec<TomlValue<T>> {
    type Concrete = Vec<T::Concrete>;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        match helper.item {
            toml_edit::Item::ArrayOfTables(array) => {
                {
                    let res: Vec<TomlValue<T>> = array
                        .iter()
                        .map(|entry| {
                            T::fill_from_toml(&mut TomlHelper::from_item(
                                &toml_edit::Item::Table(entry.clone()), //todo: can we do this without
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
            }
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

    fn v_register_errors(&self, col: &TomlCollector) {
        for v in self.iter() {
            v.register_error(col);
        }
    }

    fn into_concrete(self) -> Self::Concrete {
        self.into_iter()
            .map(|item| item.value.unwrap().into_concrete())
            .collect()
    }
}

impl<R, T: Visitor + VerifyVisitor<R> + VerifyIn<R>> VerifyVisitor<R> for Vec<TomlValue<T>> {
    fn vv_validate(self, helper: &mut TomlHelper<'_>, parent: &R) -> Self
    where
        Self: Sized + Visitor,
    {
        let v: Vec<TomlValue<T>> = self
            .into_iter()
            .map(|entry| entry.tpd_validate(helper, parent))
            .collect();
        v
    }
}
impl<R, T: Visitor + VerifyVisitor<R> + VerifyIn<R>> VerifyIn<R> for Vec<TomlValue<T>> {}

impl<T, K: From<String> + std::hash::Hash + Eq + std::fmt::Debug> Visitor
    for IndexMap<K, TomlValue<T>>
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

    fn v_register_errors(&self, col: &TomlCollector) {
        for (_key,value) in self.iter() {
            value.register_error(col);

        }

    }

    fn into_concrete(self) -> Self::Concrete {
        self.into_iter()
            .map(|(k, v)| (k, v.value.unwrap().into_concrete()))
            .collect()
    }
}

impl<K: std::hash::Hash + Eq, R, T: Visitor + VerifyVisitor<R> + VerifyIn<R>> VerifyVisitor<R>
    for IndexMap<K, TomlValue<T>>
{
    fn vv_validate(self, helper: &mut TomlHelper<'_>, parent: &R) -> Self
    where
        Self: Sized + Visitor,
    {
        let out: IndexMap<K, TomlValue<T>> = self
            .into_iter()
            .map(|(k, v)| (k, v.tpd_validate(helper, parent)))
            .collect();
        out
    }
}

impl<K: std::hash::Hash + Eq, R, T: Visitor + VerifyVisitor<R> + VerifyIn<R>> VerifyIn<R>
    for IndexMap<K, TomlValue<T>>
{
}
