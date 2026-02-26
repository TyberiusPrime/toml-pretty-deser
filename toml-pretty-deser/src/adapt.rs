use crate::traits::Visitor;
use crate::value::{TomlOr, TomlValue, TomlValueState};

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

    pub fn as_ref_post(&self) -> Option<&B> {
        match self {
            MustAdapt::PreVerify(_) => None,
            MustAdapt::PostVerify(v) => Some(v),
        }
    }
}

/// User facing 'adapt' function for MustAdapt / MustAdaptNested
pub trait MustAdaptHelper<A, B> {
    /// Adapt the value in place from `TomlValue::Ok(A)`
    /// to TomlValue<B>
    ///
    /// You use this together with #[tdp(adapt_in_verify]).
    ///
    /// Return a non-Ok `TomlValueState` to fail the conversion.
    fn adapt<F>(&mut self, map_func: F)
    where
        F: FnOnce(A) -> (B, TomlValueState),
        Self: Sized;
}

impl<A: Visitor + std::fmt::Debug, B: std::fmt::Debug> MustAdaptHelper<A, B>
    for TomlValue<MustAdapt<A, B>>
{
    fn adapt<F>(&mut self, map_func: F)
    where
        F: FnOnce(A) -> (B, TomlValueState),
        Self: Sized,
    {
        let t = self.take();
        let span = t.span;
        let context = t.context;
        *self = match (t.state, t.value, t.help) {
            (TomlValueState::NeedsFurtherValidation, Some(MustAdapt::PreVerify(v)), _) => {
                let (b, state) = map_func(v);
                TomlValue {
                    state,
                    span,
                    value: Some(MustAdapt::PostVerify(b)),
                    help: None,
                    context,
                }
            }
            (state, value, help) => TomlValue {
                state,
                span,
                value,
                help,
                context,
            },
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

impl<A: std::fmt::Debug, B: std::fmt::Debug> TomlOr<B> for TomlValue<MustAdapt<A, B>> {
    fn or(&mut self, default: B) {
        if matches!(self.state, TomlValueState::Missing { .. }) {
            let old = self.take();
            *self = Self {
                value: Some(MustAdapt::PostVerify(default)),
                state: TomlValueState::Ok,
                span: old.span,
                help: None,
                context: None,
            };
        }
    }

    fn or_with<F>(&mut self, default_func: F)
    where
        F: FnOnce() -> B,
    {
        if matches!(self.state, TomlValueState::Missing { .. }) {
            let old = self.take();
            *self = Self {
                value: Some(MustAdapt::PostVerify(default_func())),
                state: TomlValueState::Ok,
                span: old.span,
                help: None,
                context: None,
            };
        }
    }
}

impl<A: Visitor, B> TomlOr<B> for TomlValue<MustAdaptNested<A, B>> {
    fn or(&mut self, default: B) {
        if matches!(self.state, TomlValueState::Missing { .. }) {
            let old = self.take();
            *self = Self {
                value: Some(MustAdaptNested(MustAdapt::PostVerify(default))),
                state: TomlValueState::Ok,
                span: old.span,
                help: None,
                context: None,
            };
        }
    }

    fn or_with<F>(&mut self, default_func: F)
    where
        F: FnOnce() -> B,
    {
        if matches!(self.state, TomlValueState::Missing { .. }) {
            let old = self.take();
            *self = Self {
                value: Some(MustAdaptNested(MustAdapt::PostVerify(default_func()))),
                state: TomlValueState::Ok,
                span: old.span,
                help: None,
                context: None,
            };
        }
    }
}

impl<A: Visitor, B> MustAdaptHelper<A::Concrete, B> for TomlValue<MustAdaptNested<A, B>> {
    fn adapt<F>(&mut self, map_func: F)
    where
        F: FnOnce(A::Concrete) -> (B, TomlValueState),
        Self: Sized,
    {
        let t = self.take();
        let TomlValue {
            span,
            state: t_state,
            value: t_value,
            help: t_help,
            context: t_context,
        } = t;
        *self = match (t_state, t_value, t_help) {
            (
                TomlValueState::NeedsFurtherValidation,
                Some(MustAdaptNested(MustAdapt::PreVerify(v))),
                _,
            ) => {
                if v.can_concrete() {
                    let concrete = v.into_concrete();
                    let (b, state) = map_func(concrete);
                    TomlValue {
                        state,
                        span,
                        value: Some(MustAdaptNested(MustAdapt::PostVerify(b))),
                        help: None,
                        context: t_context,
                    }
                } else {
                    // Inner has errors; preserve state so errors propagate via v_register_errors
                    TomlValue {
                        state: TomlValueState::NeedsFurtherValidation,
                        value: Some(MustAdaptNested(MustAdapt::PreVerify(v))),
                        span,
                        help: None,
                        context: t_context,
                    }
                }
            }
            (state, value, help) => TomlValue {
                state,
                span,
                value,
                help,
                context: t_context,
            },
        }
    }
}
