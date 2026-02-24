use crate::collector::TomlCollector;
use crate::error::{AnnotatedError, SpannedMessage};
use crate::table_helper::TomlHelper;
use crate::value::{TomlValue, TomlValueState};

/// The error type for `VerifyIn.verify()`
pub struct ValidationFailure {
    pub message: String,
    pub help: Option<String>,
}

impl ValidationFailure {
    pub fn new<T: AsRef<str>>(message: T, help: Option<T>) -> Self {
        Self {
            message: message.as_ref().to_string(),
            help: help.map(|h| h.as_ref().to_string()),
        }
    }
}

/// The main user facing verification trait.
///
/// Implement this on your `PartialT` for the parent type
/// that it's going to see from your nested structure.
///
/// If this is a top level `Partial`, use [`Root`]
///
/// If you don't need the parent object for verification use
/// ```rust, ignore
/// use toml_pretty_deser::prelude::*;
///
/// impl <R> VerifyIn<R> for PartialT {
///
///    fn verify(&mut self,
///     helper: &mut TomlHelper<'_>,
///     parent: &R)
///     -> Result<(), (String, Option<String>)> {
///       // ...
///    }
/// }
///    ```
///
pub trait VerifyIn<Parent> {
    #[allow(unused_variables)]
    /// # Errors
    /// When the developer wants to replace this value with
    /// a `TomlValue` in failed verification state.
    fn verify(&mut self, parent: &Parent) -> Result<(), ValidationFailure>
    where
        Self: Sized + Visitor,
    {
        Ok(())
    }
}

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

    fn needs_further_validation(&self) -> bool {
        false
    }

    /// Macro-derived, recursively turn `TomlValues` into `AnnotatedError`
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
    fn vv_validate(self, parent: &Parent) -> Self
    where
        Self: Sized + Visitor,
    {
        self
    }
}

/// The empty struct passed to top level [`VerifyIn`] calls.
#[derive(Default)]
pub struct TPDRoot;

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
                span: helper.span(),
                help: None,
            }
        } else if visitor.can_concrete() {
            TomlValue::new_ok(visitor, helper.span())
        } else {
            TomlValue {
                value: Some(visitor),
                state: TomlValueState::Nested,
                span: helper.span(),
                help: None,
            }
        }
    }

    /// # Panics
    ///
    /// When ok -> value present invariant is violated
    #[must_use]
    pub fn tpd_validate<R>(self, parent: &R) -> TomlValue<T>
    where
        T: Visitor + VerifyVisitor<R> + VerifyIn<R>,
    {
        match self.state {
            TomlValueState::Ok => {
                let span = self.span;
                let mut maybe_validated =
                    self.value.expect("ok, but no value?").vv_validate(parent);
                let v = maybe_validated.verify(parent);
                match (
                    v,
                    maybe_validated.can_concrete(),
                    maybe_validated.needs_further_validation(),
                ) {
                    (Ok(()), true, _) => TomlValue::new_ok(maybe_validated, span),
                    (Ok(()), false, false) => TomlValue::new_nested(Some(maybe_validated), span),
                    (Ok(()), false, true) => TomlValue {
                        value: Some(maybe_validated),
                        state: TomlValueState::NeedsFurtherValidation,
                        span,
                        help: None,
                    },
                    (Err(ValidationFailure { message, help }), _, _) => TomlValue {
                        state: TomlValueState::ValidationFailed { message },
                        value: Some(maybe_validated),
                        span,
                        help,
                    },
                }
            }
            TomlValueState::Nested => {
                let span = self.span.clone();
                if let Some(value) = self.value {
                    let mut maybe_validated = value.vv_validate(parent);
                    let v = maybe_validated.verify(parent);
                    match (
                        v,
                        maybe_validated.can_concrete(),
                        maybe_validated.needs_further_validation(),
                    ) {
                        (Ok(()), true, _) => TomlValue::new_ok(maybe_validated, span),
                        (Ok(()), false, false) => TomlValue {
                            value: Some(maybe_validated),
                            state: TomlValueState::Nested,
                            span,
                            help: None,
                        },
                        (Ok(()), false, true) => TomlValue {
                            value: Some(maybe_validated),
                            state: TomlValueState::NeedsFurtherValidation,
                            span,
                            help: None,
                        },
                        (Err(ValidationFailure { message, help }), _, _) => TomlValue {
                            state: TomlValueState::ValidationFailed { message },
                            value: Some(maybe_validated),
                            span,
                            help,
                        },
                    }
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
            TomlValueState::Ok => {
                return;
            }
            TomlValueState::NotSet => {
                vec![AnnotatedError::unplaced(
                    "A required field was not set by the deser code. This is a bug",
                )]
            }
            TomlValueState::Nested => {
                let __ctx = col.push_help_context_opt(self.help.as_deref());
                if let Some(value) = self.value.as_ref() {
                    value.v_register_errors(col);
                }
                col.pop_help_context_to(__ctx);
                return;
            }
            TomlValueState::Missing { key } => vec![AnnotatedError::placed(
                self.span.clone(),
                &format!("Missing required key: '{key}'."),
                self.help.as_ref().map_or("", String::as_str),
            )],
            TomlValueState::MultiDefined { key, spans } => {
                let default_help =
                    format!("Use only one of the keys involved. Canonical is '{key}'.");
                let mut err = AnnotatedError::placed(
                    spans[0].clone(),
                    "Key/alias conflict (defined multiple times).",
                    self.help
                        .as_ref()
                        .map_or(default_help.as_str(), String::as_str),
                );
                for span in spans.iter().skip(1) {
                    err.add_span(span.clone(), "Also defined here");
                }
                vec![err]
            }
            TomlValueState::WrongType { expected, found } => vec![AnnotatedError::placed(
                self.span.clone(),
                &format!("Wrong type: expected {expected}, found {found}."),
                self.help
                    .as_ref()
                    .map_or("This value has the wrong type.", String::as_str),
            )],
            TomlValueState::ValidationFailed { message } => {
                // Also traverse inner value: the struct may have nested parse errors
                // (e.g. when the element was in Nested state before verify fired).
                // Push self.help so those child errors inherit it too; pop before
                // building our own error (which already carries self.help directly).
                if let Some(value) = self.value.as_ref() {
                    let __ctx = col.push_help_context_opt(self.help.as_deref());
                    value.v_register_errors(col);
                    col.pop_help_context_to(__ctx);
                }
                vec![AnnotatedError::placed(
                    self.span.clone(),
                    message,
                    self.help.as_ref().map_or("", String::as_str),
                )]
            }
            TomlValueState::UnknownKeys(unknown_keys) => {
                // Push self.help so that children (from v_register_errors) inherit it.
                let __ctx = col.push_help_context_opt(self.help.as_deref());
                if let Some(value) = self.value.as_ref() {
                    value.v_register_errors(col);
                }
                col.pop_help_context_to(__ctx);
                // Build the own unknown-key errors. These use uk.help (not self.help)
                // so we manually prepend self.help here — the outer loop will then
                // append the ancestor context on top.
                unknown_keys
                    .iter()
                    .map(|uk| {
                        let mut err =
                            AnnotatedError::placed(uk.span.clone(), "Unknown key.", &uk.help);
                        for (span, msg) in &uk.additional_spans {
                            err.add_span(span.clone(), msg);
                        }
                        if let Some(h) = &self.help {
                            match &mut err.help {
                                Some(existing) if !existing.is_empty() => {
                                    existing.push('\n');
                                    existing.push_str(h);
                                }
                                _ => err.help = Some(h.clone()),
                            }
                        }
                        err
                    })
                    .collect()
            }
            TomlValueState::Custom { spans } => {
                // Push self.help for child errors; pop before building our own error
                // (which already carries self.help directly).
                if let Some(value) = self.value.as_ref() {
                    let __ctx = col.push_help_context_opt(self.help.as_deref());
                    value.v_register_errors(col);
                    col.pop_help_context_to(__ctx);
                }
                let mut err = AnnotatedError::placed(
                    spans.iter().next().map_or(&(0..0), |x| &x.0).clone(),
                    spans
                        .iter()
                        .next()
                        .map_or_else(|| "Missing message", |x| x.1.as_str()),
                    self.help.as_ref().map_or("", String::as_str),
                );
                for (span, msg) in spans.iter().skip(1) {
                    err.add_span(span.clone(), msg);
                }
                vec![err]
            }
            TomlValueState::NeedsFurtherValidation => vec![AnnotatedError::placed(
                self.span.clone(),
                "This value was expected to receive further transformation in VerifyIn",
                self.help.as_ref().map_or(
                    "This points to a bug in the deserilization code, please report it.",
                    String::as_str,
                ),
            )],
        };

        for mut err in errs {
            // Add context spans to the error
            for context in context_spans {
                err.add_span(context.span.clone(), &context.msg);
            }
            // Append help lines pushed by ancestor Nested values.
            // Reversed so that the innermost (most-specific) help appears first.
            let help_ctx = col.get_context_help();
            if !help_ctx.is_empty() {
                let addon = help_ctx.into_iter().rev().collect::<Vec<_>>().join("\n");
                match &mut err.help {
                    Some(existing) if !existing.is_empty() => {
                        existing.push('\n');
                        existing.push_str(&addon);
                    }
                    _ => err.help = Some(addon),
                }
            }
            col.errors.borrow_mut().push(err);
        }
    }
}

impl<T> TomlValue<T> {
    /// Register errors for a leaf (non-nested) field. Does not require `T: Visitor`.
    /// Used by the macro for fields annotated with `#[tpd(with = "...")]`, where the adapter
    /// already produced the final value during `fill_from_toml` and no recursive visiting occurs.
    pub fn register_error_leaf(&self, col: &TomlCollector) {
        let context = col.get_context_spans();
        let errs: Vec<AnnotatedError> = match &self.state {
            TomlValueState::Ok | TomlValueState::Nested => return,
            TomlValueState::NotSet { .. } => {
                vec![AnnotatedError::unplaced(
                    "A required field was not set by the deser code. This is a bug",
                )]
            }
            TomlValueState::Missing { key } => vec![AnnotatedError::placed(
                self.span.clone(),
                &format!("Missing required key: '{key}'."),
                self.help.as_ref().map_or("", String::as_str),
            )],
            TomlValueState::MultiDefined { key, spans } => {
                let default_help =
                    format!("Use only one of the keys involved. Canonical is '{key}'.");
                let mut err = AnnotatedError::placed(
                    spans[0].clone(),
                    "Key/alias conflict (defined multiple times).",
                    self.help
                        .as_ref()
                        .map_or(default_help.as_str(), String::as_str),
                );
                for span in spans.iter().skip(1) {
                    err.add_span(span.clone(), "Also defined here");
                }
                vec![err]
            }
            TomlValueState::WrongType { expected, found } => vec![AnnotatedError::placed(
                self.span.clone(),
                &format!("Wrong type: expected {expected}, found {found}."),
                self.help
                    .as_ref()
                    .map_or("This value has the wrong type.", String::as_str),
            )],
            TomlValueState::ValidationFailed { message } => vec![AnnotatedError::placed(
                self.span.clone(),
                message,
                self.help.as_ref().map_or("", String::as_str),
            )],
            TomlValueState::UnknownKeys(unknown_keys) => unknown_keys
                .iter()
                .map(|uk| {
                    let mut err = AnnotatedError::placed(uk.span.clone(), "Unknown key.", &uk.help);
                    for (span, msg) in &uk.additional_spans {
                        err.add_span(span.clone(), msg);
                    }
                    err
                })
                .collect(),
            TomlValueState::Custom { spans } => {
                let mut err = AnnotatedError::placed(
                    spans.iter().next().map_or(&(0..0), |x| &x.0).clone(),
                    spans
                        .iter()
                        .next()
                        .map_or_else(|| "Missing message", |x| x.1.as_str()),
                    self.help.as_ref().map_or("", String::as_str),
                );
                for (span, msg) in spans.iter().skip(1) {
                    err.add_span(span.clone(), msg);
                }
                vec![err]
            }
            TomlValueState::NeedsFurtherValidation => vec![AnnotatedError::placed(
                self.span.clone(),
                "This value was expected to receive further transformation in VerifyIn",
                self.help.as_ref().map_or(
                    "This points to a bug in the deserilization code, please report it.",
                    String::as_str,
                ),
            )],
        };

        for mut err in errs {
            for context_span in &context {
                err.add_span(context_span.span.clone(), &context_span.msg);
            }
            // Append help lines pushed by ancestor Nested values.
            // Reversed so that the innermost (most-specific) help appears first.
            let help_ctx = col.get_context_help();
            if !help_ctx.is_empty() {
                let addon = help_ctx.into_iter().rev().collect::<Vec<_>>().join("\n");
                match &mut err.help {
                    Some(existing) if !existing.is_empty() => {
                        existing.push('\n');
                        existing.push_str(&addon);
                    }
                    _ => err.help = Some(addon),
                }
            }
            col.errors.borrow_mut().push(err);
        }
    }
}
