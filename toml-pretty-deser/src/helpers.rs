use std::{cell::RefCell, rc::Rc};

use toml_edit::Document;

use crate::{
    AnnotatedError, DeserError, FieldMatchMode, SpannedMessage, TomlCollector, TomlHelper,
    TomlValue, TomlValueState, ValidationFailure, VecMode, VerifyIn,
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
    pub fn tpd_validate<R>(self, parent: &R) -> TomlValue<T>
    where
        T: Visitor + VerifyVisitor<R> + VerifyIn<R>,
    {
        match self.state {
            TomlValueState::Ok { .. } => {
                let span = self.span();
                let mut maybe_validated =
                    self.value.expect("ok, but no value?").vv_validate(parent);
                let v = maybe_validated.verify(parent);
                match (
                    v,
                    maybe_validated.can_concrete(),
                    maybe_validated.needs_further_validation(),
                ) {
                    (Ok(()), true, _) => TomlValue::new_ok(maybe_validated, span),
                    (Ok(()), false, false) => TomlValue::new_nested(Some(maybe_validated)),
                    (Ok(()), false, true) => TomlValue {
                        value: Some(maybe_validated),
                        state: TomlValueState::NeedsFurtherValidation { span },
                    },
                    (Err(ValidationFailure { message, help }), _, _) => TomlValue {
                        state: TomlValueState::ValidationFailed {
                            span,
                            message,
                            help,
                        },
                        value: Some(maybe_validated),
                    },
                }
            }
            TomlValueState::Nested => {
                if let Some(value) = self.value {
                    let mut maybe_validated = value.vv_validate(parent);
                    maybe_validated.verify(parent).ok();
                    if maybe_validated.can_concrete() {
                        TomlValue::new_ok(maybe_validated, 0..0)
                    } else {
                        TomlValue::new_nested(Some(maybe_validated))
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
            TomlValueState::NotSet | TomlValueState::Ok { .. } => {
                return;
            }
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
            TomlValueState::NeedsFurtherValidation { span } => vec![AnnotatedError::placed(
                span.clone(),
                "This value was expected to receive further transformation in VerifyIn",
                "This points to a bug in the deserilization code, please report it.",
            )],
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
    let mut root = root.tpd_validate(&Root);
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

/// Wrapper type for #[tdp(adapt_in_verify)]
#[derive(Debug)]
pub enum MustAdapt<A, B> {
    PreVerify(A),
    PostVerify(B),
}

impl<A, B> MustAdapt<A, B> {
    pub fn unwrap_post(self) -> B {
        match self {
            Self::PreVerify(_) => panic!(
                "MustAdapt was not adapted before verification. Remained in PreVerify Type {}, but unwrap_post was called",
                std::any::type_name::<A>()
            ),
            Self::PostVerify(b) => b,
        }
    }

    pub fn unwrap_pre(self) -> A {
        match self {
            Self::PreVerify(a) => a,
            Self::PostVerify(_) => {
                panic!("MustAdapt was adapted before verification, but unwrap_pre was called.",)
            }
        }
    }
}

/// User facing 'adapt' function for MustAdapt / MustAdaptNested
pub trait MustAdaptHelper<A, S> {
    fn adapt<F>(&mut self, map_func: F)
    where
        F: FnOnce(A, std::ops::Range<usize>) -> TomlValue<S>,
        Self: Sized;
}

impl<A: Visitor + std::fmt::Debug, B: std::fmt::Debug> MustAdaptHelper<A, B>
    for TomlValue<MustAdapt<A, B>>
{
    fn adapt<F>(&mut self, map_func: F)
    where
        F: FnOnce(A, std::ops::Range<usize>) -> TomlValue<B>,
        Self: Sized,
    {
        let t = self.take();
        *self = match (t.state, t.value) {
            (TomlValueState::NeedsFurtherValidation { span }, Some(MustAdapt::PreVerify(v))) => {
                let value = map_func(v, span);
                value.map(|x| MustAdapt::PostVerify(x))
            }
            (state, value) => TomlValue { state, value },
        }
    }
}

/// Like [`MustAdapt`], but used with `#[tpd(nested, adapt_in_verify)]`.
///
/// Stores `PartialT` during parsing/verification, then calls `into_concrete()` automatically
/// before passing `T` (the concrete type) to the user's `adapt` closure.
///
/// Neither `A` nor `B` need to implement `Debug` to use this type; `Debug` is only required
/// when actually formatting the value.
pub struct MustAdaptNested<A: Visitor, B>(pub MustAdapt<A, B>);

impl<A: Visitor, B> std::fmt::Debug for MustAdaptNested<A, B> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            MustAdapt::PreVerify(_) => f
                .debug_struct("MustAdaptNested::PreVerify")
                .finish_non_exhaustive(),
            MustAdapt::PostVerify(_) => f
                .debug_struct("MustAdaptNested::PostVerify")
                .finish_non_exhaustive(),
        }
    }
}

impl<A: Visitor, B> MustAdaptHelper<A::Concrete, B> for TomlValue<MustAdaptNested<A, B>> {
    fn adapt<F>(&mut self, map_func: F)
    where
        F: FnOnce(A::Concrete, std::ops::Range<usize>) -> TomlValue<B>,
        Self: Sized,
    {
        let t = self.take();
        *self = match (t.state, t.value) {
            (
                TomlValueState::NeedsFurtherValidation { span },
                Some(MustAdaptNested(MustAdapt::PreVerify(v))),
            ) => {
                if v.can_concrete() {
                    let concrete = v.into_concrete();
                    let value = map_func(concrete, span);
                    value.map(|x| MustAdaptNested(MustAdapt::PostVerify(x)))
                } else {
                    // Inner has errors; preserve state so errors propagate via v_register_errors
                    TomlValue {
                        state: TomlValueState::NeedsFurtherValidation { span },
                        value: Some(MustAdaptNested(MustAdapt::PreVerify(v))),
                    }
                }
            }
            (state, value) => TomlValue { state, value },
        }
    }
}
