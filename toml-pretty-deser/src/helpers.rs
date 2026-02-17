use std::{cell::RefCell, rc::Rc};

use toml_edit::Document;

use crate::{
    AnnotatedError, AnnotatedErrorExt, DeserError, FieldMatchMode, SpannedMessage, TomlCollector,
    TomlHelper, TomlValue, TomlValueState, VecMode, VerifyIn,
};

/// The main parent-independent visitor trait.
/// See `impl_visitor`! for macro-derived implementations for simple types,
/// or the toml-pretty-deser-macros crate for the `#[tpd]` tagged struct implementations.
///
pub trait Visitor: Sized {
    type Concrete;

    /// Populate self from TOML, given the helper around `TomlItem`
    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self>;

    /// Macro-derived: recursively checks all `TomlValue`<_> fields are .`is_ok()`
    fn can_concrete(&self) -> bool;

    /// Macro-derived, recurisivly turn `TomlValues` into `AnnotatedError`
    fn v_register_errors(&self, col: &TomlCollector);

    /// Consume into the concrete `T`.
    ///
    ///
    /// # Panics
    /// - if !`can_concrete()`
    fn into_concrete(self) -> Self::Concrete;
}

/// The parent-dependent visitor trait. Implementations are macro-derived.
pub trait VerifyVisitor<Parent> {
    #[allow(unused_variables)]
    #[must_use]
    fn vv_validate(self, helper: &mut TomlHelper<'_>, parent: &Parent) -> Self
    where
        Self: Sized + Visitor,
    {
        self
    }
}

/// The empty struct passed to top level [`VerifyIn`] calls.
#[derive(Default)]
pub struct Root;

/// methods powering the `toml-pretty-deser-macros` crate's `#[tpd]` struct implementations.
impl<T> TomlValue<T>
where
    T: Visitor,
{
    /// called by the toml-pretty-deser-macros `fill_from_toml` implementation.
    pub fn from_visitor(visitor: T, helper: &TomlHelper<'_>) -> Self {
        if helper.has_unknown() {
            TomlValue {
                value: Some(visitor),
                state: TomlValueState::UnknownKeys(helper.unknown_spans()),
            }
        } else if visitor.can_concrete() {
            TomlValue::new_ok(visitor, helper.span())
        } else {
            TomlValue::new_nested(Some(visitor))
        }
    }

    /// # Panics
    ///
    /// When ok -> value present invariant is violated
    #[must_use]
    pub fn tpd_validate<R>(self, helper: &mut TomlHelper, parent: &R) -> TomlValue<T>
    where
        T: Visitor + VerifyVisitor<R> + VerifyIn<R>,
    {
        match self.state {
            TomlValueState::Ok { .. } => {
                let span = self.span();
                let mut maybe_validated = self
                    .value
                    .expect("ok, but no value?")
                    .vv_validate(helper, parent);
                let v = maybe_validated.verify(helper, parent);
                match (v, maybe_validated.can_concrete()) {
                    (Ok(()), true) => TomlValue::new_ok(maybe_validated, span),
                    (Ok(()), false) => TomlValue::new_nested(Some(maybe_validated)),
                    (Err((msg, hint)), _) => TomlValue {
                        state: TomlValueState::ValidationFailed {
                            span,
                            message: msg,
                            help: hint,
                        },
                        value: Some(maybe_validated),
                    },
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
                    "Key/alias conflict (defined multiple times).",
                    &format!("Use only one of the keys involved. Canonical is '{key}'."),
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
                &format!("Wrong type: expected {expected}, found {found}."),
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
            TomlValueState::UnknownKeys(unknown_keys) => {
                if let Some(value) = self.value.as_ref() {
                    value.v_register_errors(col);
                }
                unknown_keys
                    .iter()
                    .map(|uk| {
                        let mut err =
                            AnnotatedError::placed(uk.span.clone(), "Unknown key.", &uk.help);
                        for (span, msg) in &uk.additional_spans {
                            err.add_span(span.clone(), msg);
                        }
                        err
                    })
                    .collect()
            }
            TomlValueState::Custom { spans, help } => {
                if let Some(value) = self.value.as_ref() {
                    value.v_register_errors(col);
                }
                let mut err = AnnotatedError::placed(
                    spans.iter().next().map_or(&(0..0), |x| &x.0).clone(),
                    spans
                        .iter()
                        .next()
                        .map_or_else(|| "Missing message", |x| x.1.as_str()),
                    help.as_ref().map_or("", std::string::String::as_str),
                );
                for (span, msg) in spans.iter().skip(1) {
                    err.add_span(span.clone(), msg);
                }
                vec![err]
            }
        };

        for mut err in errs {
            // Add context spans to the error
            for context in context_spans {
                err.add_span(context.span.clone(), &context.msg);
            }
            col.errors.borrow_mut().push(err);
        }
    }
}

/// Helper to implement `T::tpd_from_toml` in toml-pretty-deser-macros
///
///
/// # Errors
///
/// On parsing & desererialization errors, returns `DeserError::ParsingFailure`
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
    P: Visitor + VerifyVisitor<Root> + VerifyIn<Root> + std::fmt::Debug + Default,
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
    let mut root = root.tpd_validate(&mut helper, &Root);
    if helper.has_unknown() {
        root.state = TomlValueState::UnknownKeys(helper.unknown_spans());
    }

    if root.is_ok() {
        Ok(root.value.unwrap().into_concrete())
    } else {
        root.register_error(&col);
        Err(DeserError::DeserFailure(
            helper.into_inner(&source),
            root.value.map(Box::new).unwrap_or_default(),
        ))
    }
}
