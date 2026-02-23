use indexmap::IndexMap;

use crate::{
    AsTableLikePlus, MapAndKeys, MustAdapt, MustAdaptNested, TomlCollector, TomlHelper, TomlValue,
    TomlValueState, ValidationFailure, VerifyIn, VerifyVisitor, Visitor,
};

#[macro_export]
/// Create a visitor from your `fill_from_toml` implementation. The second argument is whether to
/// also implement an empty `VerifyIn`, which is usually what we want for 'value types'
/// that can't fail.
macro_rules! impl_visitor {
    ($ty:ty, |$helper:ident| $fill_from_toml:expr) => {
        $crate::impl_visitor!($ty, true, |$helper| $fill_from_toml);
    };
    ($ty:ty, false, |$helper:ident| $fill_from_toml:expr) => {
        $crate::impl_visitor!(@inner $ty, |$helper| $fill_from_toml);
    };
    ($ty:ty, true, |$helper:ident| $fill_from_toml:expr) => {
        $crate::impl_visitor!(@inner $ty, |$helper| $fill_from_toml);
        impl<R> $crate::VerifyIn<R> for $ty {}
    };
    (@inner $ty:ty, |$helper:ident| $fill_from_toml:expr) => {
        impl $crate::Visitor for $ty {
            type Concrete = $ty;
            fn fill_from_toml($helper: &mut $crate::TomlHelper<'_>) -> $crate::TomlValue<Self> {
                $fill_from_toml
            }
            fn can_concrete(&self) -> bool {
                true
            }
            fn v_register_errors(&self, _col: &$crate::TomlCollector) {}
            fn into_concrete(self) -> Self::Concrete {
                self
            }
        }
        impl<R> $crate::VerifyVisitor<R> for $ty {}
    };
}

macro_rules! impl_visitor_for_int {
    ($ty:ty) => {
        impl_visitor!($ty, |helper| {
            match helper.item.as_integer() {
                Some(v) => match TryInto::<$ty>::try_into(v) {
                    Ok(v) => {
                        return $crate::TomlValue::new_ok(v, helper.span());
                    }
                    Err(_) => {
                        return $crate::TomlValue::new_validation_failed(
                            helper.span(),
                            format!("integer ({}), range exceeded", stringify!($ty)),
                            Some(format!(
                                "Use a number between {} and {} inclusive.",
                                <$ty>::MIN,
                                (<$ty>::MAX as u128).min(i64::MAX as u128)
                            )),
                        );
                    }
                },
                None => {
                    return $crate::TomlValue::new_wrong_type(
                        &helper.item,
                        helper.span(),
                        stringify!($ty),
                    );
                }
            }
        });
    };
}

// Invocations for all integer types
impl_visitor_for_int!(i8);
impl_visitor_for_int!(i16);
impl_visitor_for_int!(i32);
impl_visitor_for_int!(i64);
//impl_visitor_for_int!(i128); toml is i64 anyway
impl_visitor_for_int!(isize);
impl_visitor_for_int!(u8);
impl_visitor_for_int!(u16);
impl_visitor_for_int!(u32);
impl_visitor_for_int!(u64);
//impl_visitor_for_int!(u128); toml is i64 anyway
impl_visitor_for_int!(usize);

impl_visitor!(bool, |helper| {
    match helper.item.as_bool() {
        Some(v) => TomlValue::new_ok(v, helper.span()),
        None => TomlValue::new_wrong_type(helper.item, helper.span(), "boolean"),
    }
});

impl_visitor!(f64, |helper| {
    match helper.item.as_float() {
        Some(v) => TomlValue::new_ok(v, helper.span()),
        None => {
            if let Some(x) = helper.item.as_integer() {
                TomlValue::new_ok(x as f64, helper.span())
            } else {
                TomlValue::new_wrong_type(helper.item, helper.span(), "float")
            }
        }
    }
});

impl_visitor!((), |helper| TomlValue::new_ok((), helper.span()));

impl_visitor!(toml_edit::Item, |helper| TomlValue::new_ok(
    helper.item.clone(),
    helper.span()
));

/// implement a Visitor on a value that implements `From<String>`
#[macro_export]
macro_rules! impl_visitor_for_from_str {
    ($ty:ty) => {
        $crate::impl_visitor!($ty, |helper| {
            match helper.item.as_str() {
                Some(v) => $crate::TomlValue::new_ok(v.into(), helper.span()),
                None => $crate::TomlValue::new_wrong_type(&helper.item, helper.span(), "string"),
            }
        });
    };
}

/// implement a Visitor on a value that implements `TryFrom<String>`
#[macro_export]
macro_rules! impl_visitor_for_try_from_str {
    ($ty:ty, $help:expr) => {
        $crate::impl_visitor!($ty, |helper| {
            match helper.item.as_str() {
                Some(v) => match v.try_into() {
                    Ok(v) => $crate::TomlValue::new_ok(v, helper.span()),
                    Err(_) => $crate::TomlValue::new_validation_failed(
                        helper.span(),
                        "Unconvertible string".to_string(),
                        Some($help.to_string()),
                    ),
                },
                None => $crate::TomlValue::new_wrong_type(&helper.item, helper.span(), "string"),
            }
        });
    };
}

impl_visitor_for_from_str!(String);
impl_visitor_for_from_str!(std::path::PathBuf);

impl<T: Visitor> Visitor for Option<T> {
    type Concrete = Option<T::Concrete>;

    #[mutants::skip]
    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let inner = T::fill_from_toml(helper);
        match inner.state {
            TomlValueState::Ok => TomlValue::new_ok(Some(inner.value.unwrap()), helper.span()),
            TomlValueState::Missing { .. } => unreachable!(),
            _ => inner.convert_failed_type(),
        }
    }

    fn can_concrete(&self) -> bool {
        self.as_ref().is_none_or(<T as Visitor>::can_concrete)
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        if let Some(v) = self {
            v.v_register_errors(col)
        }
    }

    fn into_concrete(self) -> Option<T::Concrete> {
        self.map(<T as Visitor>::into_concrete)
    }
}

impl<R, T: Visitor + VerifyVisitor<R>> VerifyVisitor<R> for Option<T> {
    fn vv_validate(self, parent: &R) -> Self {
        match self {
            Some(v) => Some(v.vv_validate(parent)),
            None => None,
        }
    }
}

impl<T: Visitor> Visitor for Box<T> {
    type Concrete = Box<T::Concrete>;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let inner = T::fill_from_toml(helper);
        inner.map_any(|x| Box::new(x))
    }

    fn can_concrete(&self) -> bool {
        self.as_ref().can_concrete()
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.as_ref().v_register_errors(col);
    }

    fn into_concrete(self) -> Box<T::Concrete> {
        Box::new((*self).into_concrete())
    }
}
impl<R, T: Visitor + VerifyVisitor<R>> VerifyVisitor<R> for Box<T> {
    fn vv_validate(self, parent: &R) -> Self {
        Box::new((*self).vv_validate(parent))
    }
}
impl<R, T: Visitor + VerifyVisitor<R>> VerifyIn<R> for Box<T> {}

impl<R, T: VerifyIn<R> + Visitor> VerifyIn<R> for Option<T> {
    fn verify(&mut self, parent: &R) -> Result<(), crate::ValidationFailure>
    where
        Self: Sized + Visitor,
    {
        if let Some(inner) = self {
            inner.verify(parent)?;
        }
        Ok(())
    }
}

impl<T: Visitor> Visitor for Vec<TomlValue<T>> {
    type Concrete = Vec<T::Concrete>;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        match &helper.item {
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
                    if res.can_concrete() {
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
                if res.can_concrete() {
                    TomlValue::new_ok(res, helper.span())
                } else {
                    TomlValue::new_nested(Some(res))
                }
            }
            toml_edit::Item::Value(v) if helper.col.vec_mode.single_ok() => {
                let res = vec![T::fill_from_toml(&mut TomlHelper::from_item(
                    &toml_edit::Item::Value(v.clone()), //todo: can we do this without clone
                    helper.col.clone(),
                ))];
                if res.can_concrete() {
                    TomlValue::new_ok(res, helper.span())
                } else {
                    TomlValue::new_nested(Some(res))
                }
            }
            _ => TomlValue::new_wrong_type(helper.item, helper.span(), "array"),
        }
    }

    fn can_concrete(&self) -> bool {
        self.iter().all(TomlValue::is_ok)
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        for v in self {
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
    fn vv_validate(self, parent: &R) -> Self
    where
        Self: Sized + Visitor,
    {
        let v: Vec<TomlValue<T>> = self
            .into_iter()
            .map(|entry| entry.tpd_validate(parent))
            .collect();
        v
    }
}
impl<R, T: Visitor + VerifyVisitor<R> + VerifyIn<R>> VerifyIn<R> for Vec<TomlValue<T>> {}

impl<T, K: From<String> + std::hash::Hash + Eq + std::fmt::Debug> Visitor for MapAndKeys<K, T>
where
    T: Visitor,
{
    type Concrete = IndexMap<K, T::Concrete>;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        match helper.item.as_table_like_plus() {
            Some(table) => {
                let mut result = IndexMap::new();
                let mut all_ok = true;
                let mut keys = Vec::new();
                for (key, value) in table.iter() {
                    let key_span = table
                        .key(key)
                        .expect("Just queried!")
                        .span()
                        .unwrap_or(0..0);
                    keys.push(TomlValue::new_ok(key.to_string(), key_span));
                    let key: K = key.to_string().into();
                    let mut value_helper = TomlHelper::from_item(value, helper.col.clone());
                    let deserialized_value = T::fill_from_toml(&mut value_helper);
                    if !deserialized_value.is_ok() {
                        all_ok = false;
                    }
                    result.insert(key, deserialized_value);
                }
                let result = MapAndKeys { map: result, keys };
                if all_ok {
                    TomlValue::new_ok(result, helper.span())
                } else {
                    TomlValue::new_nested(Some(result))
                }
            }
            None => TomlValue::new_wrong_type(helper.item, helper.span(), "table"),
        }
    }

    fn can_concrete(&self) -> bool {
        self.map.values().all(TomlValue::is_ok) && self.keys.iter().all(TomlValue::is_ok)
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        for (_key, value) in &self.map {
            value.register_error(col);
        }
        for key in self.keys.iter() {
            key.register_error(col);
        }
    }

    fn into_concrete(self) -> Self::Concrete {
        self.map
            .into_iter()
            .map(|(k, v)| (k, v.value.unwrap().into_concrete()))
            .collect()
    }
}

impl<K: std::hash::Hash + Eq, R, T: Visitor + VerifyVisitor<R> + VerifyIn<R>> VerifyVisitor<R>
    for MapAndKeys<K, T>
{
    fn vv_validate(self, parent: &R) -> Self
    where
        Self: Sized + Visitor,
    {
        let map: IndexMap<K, TomlValue<T>> = self
            .map
            .into_iter()
            .map(|(k, v)| (k, v.tpd_validate(parent)))
            .collect();
        let keys = self
            .keys
            .into_iter()
            .map(|k| k.tpd_validate(parent))
            .collect();
        MapAndKeys { map, keys }
    }
}

impl<K: std::hash::Hash + Eq, R, T: Visitor + VerifyVisitor<R> + VerifyIn<R>> VerifyIn<R>
    for MapAndKeys<K, T>
{
}

impl<A: Visitor + std::fmt::Debug, B: std::fmt::Debug> Visitor for MustAdapt<A, B> {
    type Concrete = B;

    #[mutants::skip]
    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let pre_verify: TomlValue<A> = A::fill_from_toml(helper);
        pre_verify.map(|v| MustAdapt::PreVerify(v))
    }

    fn can_concrete(&self) -> bool {
        matches!(self, MustAdapt::PostVerify(_))
    }

    fn needs_further_validation(&self) -> bool {
        matches!(self, Self::PreVerify(_))
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        match self {
            MustAdapt::PreVerify(v) => v.v_register_errors(col),
            MustAdapt::PostVerify(_) => {}
        }
    }

    fn into_concrete(self) -> B {
        match self {
            MustAdapt::PreVerify(_) => panic!(
                "can_concrete invariant violated, was pre_verify, can_concrete checks for post_verify. Self={self:?}"
            ),
            MustAdapt::PostVerify(v) => v,
        }
    }
}

impl<R, A: Visitor + std::fmt::Debug + VerifyVisitor<R>, B: std::fmt::Debug> VerifyVisitor<R>
    for MustAdapt<A, B>
{
    fn vv_validate(self, parent: &R) -> Self {
        let res = match self {
            MustAdapt::PreVerify(v) => MustAdapt::PreVerify(v.vv_validate(parent)),
            MustAdapt::PostVerify(_) => unreachable!(),
        };
        res
    }
}

impl<R, A: Visitor + std::fmt::Debug + VerifyIn<R>, B: std::fmt::Debug> VerifyIn<R>
    for MustAdapt<A, B>
{
    fn verify(&mut self, parent: &R) -> Result<(), ValidationFailure>
    where
        Self: Sized + Visitor,
    {
        match self {
            MustAdapt::PreVerify(v) => v.verify(parent),
            MustAdapt::PostVerify(_) => unreachable!(),
        }
    }
}

impl<A: Visitor, B> Visitor for MustAdaptNested<A, B> {
    type Concrete = B;

    #[mutants::skip]
    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        A::fill_from_toml(helper).map(|a| MustAdaptNested(MustAdapt::PreVerify(a)))
    }

    fn can_concrete(&self) -> bool {
        matches!(self.0, MustAdapt::PostVerify(_))
    }

    fn needs_further_validation(&self) -> bool {
        matches!(self.0, MustAdapt::PreVerify(_))
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        if let MustAdapt::PreVerify(v) = &self.0 {
            v.v_register_errors(col);
        }
    }

    fn into_concrete(self) -> B {
        match self.0 {
            MustAdapt::PreVerify(_) => panic!("can_concrete invariant violated"),
            MustAdapt::PostVerify(v) => v,
        }
    }
}

impl<R, A: Visitor + VerifyVisitor<R>, B> VerifyVisitor<R> for MustAdaptNested<A, B> {
    fn vv_validate(self, parent: &R) -> Self {
        let inner = match self.0 {
            MustAdapt::PreVerify(v) => MustAdapt::PreVerify(v.vv_validate(parent)),
            MustAdapt::PostVerify(_) => unreachable!(),
        };
        MustAdaptNested(inner)
    }
}

impl<R, A: Visitor + VerifyIn<R>, B> VerifyIn<R> for MustAdaptNested<A, B> {
    fn verify(&mut self, parent: &R) -> Result<(), ValidationFailure>
    where
        Self: Sized + Visitor,
    {
        match &mut self.0 {
            MustAdapt::PreVerify(v) => v.verify(parent),
            MustAdapt::PostVerify(_) => unreachable!(),
        }
    }
}
