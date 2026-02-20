use crate::traits::Visitor;
use crate::value::{TomlValue, TomlValueState};

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
pub trait MustAdaptHelper<A, B> {
    /// Adapt the value in place from `TomlValue::Ok(A)`
    /// to TomlValue<B>
    ///
    /// You use this together with #[tdp(adapt_in_verify]).
    ///
    /// Does not need to return an Ok TomlValue,
    /// failing the conversion is ok.
    fn adapt<F>(&mut self, map_func: F)
    where
        F: FnOnce(A, std::ops::Range<usize>) -> TomlValue<B>,
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
        *self = match (t.state, t.value, t.help) {
            (TomlValueState::NeedsFurtherValidation { span }, Some(MustAdapt::PreVerify(v)), _) => {
                let value = map_func(v, span);
                value.map(|x| MustAdapt::PostVerify(x))
            }
            (state, value, help) => TomlValue { state, value, help },
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
        *self = match (t.state, t.value, t.help) {
            (
                TomlValueState::NeedsFurtherValidation { span },
                Some(MustAdaptNested(MustAdapt::PreVerify(v))),
                _,
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
                        help: None,
                    }
                }
            }
            (state, value, help) => TomlValue { state, value, help },
        }
    }
}
