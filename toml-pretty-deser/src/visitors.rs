use indexmap::IndexMap;

use crate::{
    AsTableLikePlus, TomlCollector, TomlHelper, TomlValue, VerifyIn, VerifyVisitor,
    helpers::Visitor,
};

#[macro_export]
macro_rules! impl_visitor {
    ($ty:ty, |$helper:ident| $fill_from_toml:expr) => {
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
        impl<R> $crate::VerifyIn<R> for $ty {}
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
                                (<$ty>::MAX as usize).min(i64::MAX as usize)
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
        None => TomlValue::new_wrong_type(&helper.item, helper.span(), "boolean"),
    }
});

impl_visitor!(f64, |helper| {
    match helper.item.as_float() {
        Some(v) => TomlValue::new_ok(v, helper.span()),
        None => TomlValue::new_wrong_type(&helper.item, helper.span(), "float"),
    }
});

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

    fn fill_from_toml(_helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        unreachable!(); // since we always start with a concrete TomlValue<T>
        // and then convert the missing into Some(None) in into_optional
    }

    fn can_concrete(&self) -> bool {
        unreachable!("or is it?");
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

impl<T: Visitor> Visitor for Box<T> {
    type Concrete = Box<T::Concrete>;

    fn fill_from_toml(helper: &mut TomlHelper<'_>) -> TomlValue<Self> {
        let inner = T::fill_from_toml(helper);
        dbg!(&inner);
        inner.map_any(|x| Box::new(x))
    }

    fn can_concrete(&self) -> bool {
        unreachable!("or is it?");
    }

    fn v_register_errors(&self, col: &TomlCollector) {
        self.as_ref().v_register_errors(col)
    }

    fn into_concrete(self) -> Box<T::Concrete> {
        Box::new((*self).into_concrete())
    }
}
impl<R, T: Visitor + VerifyVisitor<R>> VerifyVisitor<R> for Box<T> {
    fn vv_validate(self, helper: &mut TomlHelper<'_>, parent: &R) -> Self {
        Box::new((*self).vv_validate(helper, parent))
    }
}

impl<R, T: VerifyIn<R>> VerifyIn<R> for Option<T> {}

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
        for (_key, value) in self.iter() {
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
