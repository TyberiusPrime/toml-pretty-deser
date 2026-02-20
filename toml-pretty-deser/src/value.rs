use std::ops::Range;

/// Inner struct for `TomlValue::UnknownKeys`
#[derive(Debug, Clone)]
pub struct UnknownKey {
    pub key: String, // captured for user code
    pub span: Range<usize>,
    pub help: String,
    pub additional_spans: Vec<(Range<usize>, String)>,
}

/// Parameter to `tpd_from_toml` that controls whether
/// single values in lieu of an array are ok.
#[derive(Clone, Debug)]
pub enum VecMode {
    /// Accept single values in lieu of ```[value]```
    SingleOk,
    /// Vecs must be TOML arrays. Always.
    Strict,
}

impl VecMode {
    pub(crate) fn single_ok(&self) -> bool {
        matches!(self, VecMode::SingleOk)
    }
}

/// Captures precisely what went wrong
#[derive(Debug, Clone)]
pub enum TomlValueState {
    /// This value has not been set yet
    NotSet,
    /// This value was missing - and that's a problem
    Missing {
        key: String,
        parent_span: Range<usize>,
    },
    /// This value was defined more than once
    /// possibly using aliases.
    /// Spans point to all definitions.
    MultiDefined {
        key: String,
        spans: Vec<Range<usize>>,
    },
    /// This value had the wrong type
    WrongType {
        span: Range<usize>,
        expected: &'static str,
        found: &'static str,
    },
    /// This value had the right type, but failed validation
    ValidationFailed {
        span: Range<usize>,
        message: String,
    },
    /// There were one-or-more unknown keys within this table.
    UnknownKeys(Vec<UnknownKey>),
    /// This is a container, and one of it's children is in an error state
    Nested,
    /// A user defined error with multiple spans
    Custom {
        spans: Vec<(Range<usize>, String)>,
    },
    /// used in #tdp(adapt_in_verify(..)]
    NeedsFurtherValidation { span: Range<usize> },
    /// This value was deserialized correctly.
    Ok { span: Range<usize> },
}

/// The Result+Option representation of a TOML value that we will have
/// attempted to deserialize. Eventually.
///
/// The main component of `PartialT`
#[derive(Debug)]
pub struct TomlValue<T> {
    /// Was this value deserialized successfully,
    /// and if not, why not
    pub state: TomlValueState,
    /// Value is independent of the state,
    /// so that containers can pinpoint which of their values failed.
    pub value: Option<T>,
    /// Optional help text for error reporting.
    /// Applies to any non-Ok state.
    pub help: Option<String>,
}

impl<T> TomlValue<T> {
    /// Create a new `TomlValue` in the Ok state with the given value and span.
    #[must_use]
    pub const fn new_ok(value: T, span: Range<usize>) -> Self {
        Self {
            value: Some(value),
            state: TomlValueState::Ok { span },
            help: None,
        }
    }

    /// Create a new custom `TomlValue` error with one or multiple spans
    /// for errors that don't fit the framework
    ///
    ///
    /// # Panics
    /// when spans is empty
    #[must_use]
    pub fn new_custom(
        value: Option<T>,
        spans: Vec<(Range<usize>, String)>,
        help: Option<&str>,
    ) -> Self {
        assert!(
            !spans.is_empty(),
            "new_custom should have at least one span"
        );
        Self {
            value,
            state: TomlValueState::Custom { spans },
            help: help.map(ToString::to_string),
        }
    }

    /// Create a new `TomlValue` in the Missing state with the given parent span.
    #[must_use]
    pub const fn new_empty_missing(parent_span: Range<usize>) -> Self {
        Self {
            value: None,
            state: TomlValueState::Missing {
                key: String::new(),
                parent_span,
            },
            help: None,
        }
    }
    /// Create a new `TomlValue` with a `ValidationFailed` state.
    #[must_use]
    pub const fn new_validation_failed(
        span: Range<usize>,
        message: String,
        help: Option<String>,
    ) -> Self {
        Self {
            value: None,
            state: TomlValueState::ValidationFailed { span, message },
            help,
        }
    }

    /// Create a new `TomlValue` than reflects a wrong type being used in the TOML
    #[must_use]
    pub fn new_wrong_type(
        item: &toml_edit::Item,
        parent_span: Range<usize>,
        expected: &'static str,
    ) -> Self {
        Self {
            value: None,
            state: TomlValueState::WrongType {
                span: item.span().unwrap_or(parent_span),
                expected,
                found: item.type_name(),
            },
            help: None,
        }
    }

    /// Create a new `TomlValue` in nested error state.
    #[must_use]
    pub fn new_nested(value: Option<T>) -> Self {
        Self {
            value,
            state: TomlValueState::Nested {},
            help: None,
        }
    }

    /// Convert a `TomlValue<T>` into a `TomlValue<Option<T>`,
    /// and promote Missing from an error state into `Ok`
    pub fn into_optional(self) -> TomlValue<Option<T>> {
        match self.state {
            TomlValueState::Ok { span } => TomlValue {
                value: Some(self.value),
                state: TomlValueState::Ok { span },
                help: self.help,
            },
            TomlValueState::Missing {
                key: _,
                parent_span,
            } => TomlValue {
                value: Some(None),
                state: TomlValueState::Ok { span: parent_span },
                help: None,
            },
            TomlValueState::Nested => TomlValue {
                value: Some(self.value),
                state: TomlValueState::Nested {},
                help: self.help,
            },
            _ => TomlValue {
                value: None,
                state: self.state,
                help: self.help,
            },
        }
    }

    /// Change the inner value if state is `TomlValueState::Ok`,
    /// otherwise adapt the error type but lose the value.
    /// # Panics
    /// When the ok -> value present invariant is violated
    pub fn map<R, F>(self, map_function: F) -> TomlValue<R>
    where
        F: FnOnce(T) -> R,
    {
        match &self.state {
            TomlValueState::Ok { span } => {
                TomlValue::new_ok(map_function(self.value.unwrap()), span.clone())
            }
            _ => self.convert_failed_type(),
        }
    }

    /// Change the inner value no matter if we're ok or not.
    pub fn map_any<R, F>(self, map_function: F) -> TomlValue<R>
    where
        F: FnOnce(T) -> R,
    {
        TomlValue {
            state: self.state,
            value: self.value.map(map_function),
            help: self.help,
        }
    }

    /// Adapt failed types, eating the value
    ///
    /// # Panics
    ///
    /// When called on an ok `TomlValue`
    pub fn convert_failed_type<S>(&self) -> TomlValue<S> {
        match &self.state {
            TomlValueState::Ok { span } => {
                panic!("called convert_failed_type on a TomlValue that is Ok. Span was: {span:?}")
            }
            _ => TomlValue {
                value: None,
                state: self.state.clone(),
                help: self.help.clone(),
            },
        }
    }

    /// take the value out of the `TomlValue`, leaving a `TomlValueState::NotSet` and None in place.
    #[must_use]
    pub fn take(&mut self) -> Self {
        std::mem::replace(
            self,
            TomlValue {
                value: None,
                state: TomlValueState::NotSet,
                help: None,
            },
        )
    }

    /// Set help text on this `TomlValue`.
    pub fn set_help(&mut self, help: impl Into<String>) {
        self.help = Some(help.into());
    }

    /// Is this `TomlValue` in the Ok state?
    pub fn is_ok(&self) -> bool {
        matches!(self.state, TomlValueState::Ok { .. })
    }

    /// Get a reference to the inner value iff this `TomlValue` is in the Ok state, otherwise None.
    pub fn as_ref(&self) -> Option<&T> {
        match self.state {
            TomlValueState::Ok { .. } => self.value.as_ref(),
            _ => None,
        }
    }

    /// Get a mutable reference to the inner value iff this `TomlValue` is in the Ok state, otherwise None.
    pub fn as_mut(&mut self) -> Option<&mut T> {
        match self.state {
            TomlValueState::Ok { .. } => self.value.as_mut(),
            _ => None,
        }
    }

    /// Retrieve the primary span in the input TOML source.
    /// If no such span is available, return the 0..0 span.
    ///
    ///
    /// # Panics
    ///
    /// When called on `UnknownKeys`
    /// When called on a Custom without spans
    pub fn span(&self) -> Range<usize> {
        match &self.state {
            TomlValueState::Ok { span }
            | TomlValueState::Missing {
                parent_span: span, ..
            }
            | TomlValueState::WrongType { span, .. }
            | TomlValueState::ValidationFailed { span, .. } => span.clone(),
            TomlValueState::NotSet | TomlValueState::Nested => 0..0,
            TomlValueState::MultiDefined { spans, .. } => spans[0].clone(), //just return the first one
            TomlValueState::UnknownKeys(_) => panic!("don't call span on UnknownKeys?"),
            TomlValueState::Custom { spans, .. } => spans[0].0.clone(),
            TomlValueState::NeedsFurtherValidation { span, .. } => span.clone(),
        }
    }

    /// Verify this TOML value
    ///
    /// If the value was 'Ok', call `verification_func`
    /// and replace it on Err in-place with a `ValidationFailed` state.
    ///
    /// Non Ok `TomlValues`  are left as is.
    ///
    #[allow(clippy::missing_panics_doc)]
    pub fn verify<F>(&mut self, verification_func: F)
    where
        F: FnOnce(&T) -> Result<(), (String, Option<String>)>,
    {
        match &self.state {
            TomlValueState::Ok { span } => match verification_func(
                self.value
                    .as_ref()
                    .expect("None value on TomlValueState::Ok"),
            ) {
                Ok(()) => {
                    //unchanged
                }
                Err((msg, help)) => {
                    self.value = None;
                    self.state = TomlValueState::ValidationFailed {
                        span: span.clone(),
                        message: msg,
                    };
                    self.help = help;
                }
            },
            _ => {
                //unchanged
            }
        }
    }

    /// Replace the value with `default`if it was `Missing`
    pub fn or(&mut self, default: T) {
        match &self.state {
            TomlValueState::Missing { .. } => *self = Self {
                value: Some(default),
                state: TomlValueState::Ok { span: 0..0 },
                help: None,
            },
            _ => {},
        }
    }

    /// Replace the value with the result of `default_func` if it was `Missing`
    pub fn or_with<F>(&mut self, default_func: F)
    where
        F: FnOnce() -> T,
    {
        match &self.state {
            TomlValueState::Missing { .. } => {
                *self = Self {
                value: Some(default_func()),
                state: TomlValueState::Ok { span: 0..0 },
                help: None,
            }},
            _ => {},
        }
    }

    /// Replace the value with `T::default()` if it was `Missing`
    pub fn or_default(&mut self)
    where
        T: Default,
    {
        self.or_with(Default::default)
    }
}

/// `TomlValues` default to `NotSet`
impl<T> Default for TomlValue<T> {
    fn default() -> Self {
        TomlValue {
            value: None,
            state: TomlValueState::NotSet,
            help: None,
        }
    }
}
