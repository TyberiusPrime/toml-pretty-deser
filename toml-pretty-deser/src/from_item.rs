use super::{TomlCollector, TomlValue, TomlValueState, VecMode};
use indexmap::IndexMap;
use std::ops::Range;
/// Conversion for arbitrary types. Implement this for your custom types.
pub trait FromTomlItem {
    fn from_toml_item(
        item: &toml_edit::Item,
        parent_span: Range<usize>,
        col: &TomlCollector,
    ) -> TomlValue<Self>
    where
        Self: Sized;
}

macro_rules! impl_from_toml_item_integer {
    ($ty:ty, $name:expr) => {
        impl FromTomlItem for $ty {
            fn from_toml_item(
                item: &toml_edit::Item,
                parent_span: Range<usize>,
                _col: &TomlCollector,
            ) -> TomlValue<Self> {
                match item {
                    toml_edit::Item::None => unreachable!(),
                    toml_edit::Item::Value(toml_edit::Value::Integer(formatted)) => {
                        let value_i64 = *formatted.value();
                        let min_value: i64 = <$ty>::MIN.try_into().expect(
                            "Minimum value not representable in i64. Data type exceeds TOML",
                        );
                        let max_value: i64 = <$ty>::MAX.try_into().expect(
                            "Maximum value not representable in i64. Data type exceeds TOML",
                        );
                        if value_i64 < min_value || value_i64 > max_value {
                            TomlValue {
                                value: None,
                                state: TomlValueState::ValidationFailed {
                                    span: formatted.span().unwrap_or(parent_span.clone()),
                                    message: "Integer out of range.".to_string(),
                                    help: Some(format!("Accepted: {}..{}", <$ty>::MIN, <$ty>::MAX)),
                                },
                            }
                        } else {
                            TomlValue {
                                value: Some(
                                    value_i64
                                        .try_into()
                                        .expect("We just checked wether it's in target range"),
                                ),
                                state: TomlValueState::Ok {
                                    span: formatted.span().unwrap_or(parent_span.clone()),
                                },
                            }
                        }
                    }
                    toml_edit::Item::Value(value) => TomlValue {
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: $name,
                            found: value.type_name(),
                        },
                    },
                    toml_edit::Item::Table(value) => TomlValue {
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: $name,
                            found: "table",
                        },
                    },
                    toml_edit::Item::ArrayOfTables(value) => TomlValue {
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: $name,
                            found: "array of tables",
                        },
                    },
                }
            }
        }
    };
}

impl_from_toml_item_integer!(i8, "i8");
impl_from_toml_item_integer!(u8, "u8");
impl_from_toml_item_integer!(u16, "u16");
impl_from_toml_item_integer!(i16, "i16");
impl_from_toml_item_integer!(i32, "i32");
impl_from_toml_item_integer!(u32, "u32");
impl_from_toml_item_integer!(i64, "i64");
impl_from_toml_item_integer!(isize, "isize");

// u64 and usize need special handling because TOML only supports i64.
// The valid range is 0..i64::MAX (0..2^63-1), not the full u64 range.
macro_rules! impl_from_toml_item_unsigned_large {
    ($ty:ty, $name:expr) => {
        impl FromTomlItem for $ty {
            fn from_toml_item(
                item: &toml_edit::Item,
                parent_span: Range<usize>,
                _col: &TomlCollector,
            ) -> TomlValue<Self> {
                match item {
                    toml_edit::Item::None => unreachable!(),
                    toml_edit::Item::Value(toml_edit::Value::Integer(formatted)) => {
                        let value_i64 = *formatted.value();
                        // TOML only supports i64, so valid range for unsigned is 0..=i64::MAX
                        if value_i64 < 0 {
                            TomlValue {
                                value: None,
                                state: TomlValueState::ValidationFailed {
                                    span: formatted.span().unwrap_or(parent_span.clone()),
                                    message: "Integer out of range.".to_string(),
                                    help: Some(format!(
                                        "Accepted: 0..{} (TOML only supports i64)",
                                        i64::MAX
                                    )),
                                },
                            }
                        } else {
                            TomlValue {
                                value: Some(value_i64 as $ty),
                                state: TomlValueState::Ok {
                                    span: formatted.span().unwrap_or(parent_span.clone()),
                                },
                            }
                        }
                    }
                    toml_edit::Item::Value(value) => TomlValue {
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: $name,
                            found: value.type_name(),
                        },
                    },
                    toml_edit::Item::Table(value) => TomlValue {
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: $name,
                            found: "table",
                        },
                    },
                    toml_edit::Item::ArrayOfTables(value) => TomlValue {
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: $name,
                            found: "array of tables",
                        },
                    },
                }
            }
        }
    };
}

impl_from_toml_item_unsigned_large!(u64, "u64");
impl_from_toml_item_unsigned_large!(usize, "usize");

macro_rules! impl_from_toml_item_value {
    ($ty:ty, $name:expr, $variant:ident) => {
        impl FromTomlItem for $ty {
            fn from_toml_item(
                item: &toml_edit::Item,
                parent_span: Range<usize>,
                _col: &TomlCollector,
            ) -> TomlValue<Self> {
                match item {
                    toml_edit::Item::None => unreachable!(),
                    toml_edit::Item::Value(toml_edit::Value::$variant(formatted)) => {
                        let value = formatted.value();
                        TomlValue {
                            value: Some(value.clone()),
                            state: TomlValueState::Ok {
                                span: formatted.span().unwrap_or(parent_span.clone()),
                            },
                        }
                    }
                    toml_edit::Item::Value(value) => TomlValue {
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: $name,
                            found: value.type_name(),
                        },
                    },
                    toml_edit::Item::Table(value) => TomlValue {
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: $name,
                            found: "table",
                        },
                    },
                    toml_edit::Item::ArrayOfTables(value) => TomlValue {
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: $name,
                            found: "array of tables",
                        },
                    },
                }
            }
        }
    };
}

impl_from_toml_item_value!(bool, "bool", Boolean);
impl_from_toml_item_value!(String, "String", String);

// Custom implementation for f64 that accepts both Float and Integer values
impl FromTomlItem for f64 {
    fn from_toml_item(
        item: &toml_edit::Item,
        parent_span: Range<usize>,
        _col: &TomlCollector,
    ) -> TomlValue<Self> {
        match item {
            toml_edit::Item::None => unreachable!(),
            toml_edit::Item::Value(toml_edit::Value::Float(formatted)) => {
                let value = formatted.value();
                TomlValue {
                    value: Some(*value),
                    state: TomlValueState::Ok {
                        span: formatted.span().unwrap_or(parent_span.clone()),
                    },
                }
            }
            // Accept integers as f64 - TOML parses whole numbers as Integer, not Float
            toml_edit::Item::Value(toml_edit::Value::Integer(formatted)) => {
                let value = formatted.value();
                TomlValue {
                    value: Some(*value as f64),
                    state: TomlValueState::Ok {
                        span: formatted.span().unwrap_or(parent_span.clone()),
                    },
                }
            }
            toml_edit::Item::Value(value) => TomlValue {
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "Float",
                    found: value.type_name(),
                },
            },
            toml_edit::Item::Table(value) => TomlValue {
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "Float",
                    found: "table",
                },
            },
            toml_edit::Item::ArrayOfTables(value) => TomlValue {
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "Float",
                    found: "array of tables",
                },
            },
        }
    }
}

impl FromTomlItem for toml_edit::Item {
    #[mutants::skip] //unreachable
    fn from_toml_item(
        item: &toml_edit::Item,
        parent_span: Range<usize>,
        _col: &TomlCollector,
    ) -> TomlValue<Self> {
        match item {
            Self::None => unreachable!(),
            _ => TomlValue {
                value: Some(item.clone()),
                state: TomlValueState::Ok {
                    span: item.span().unwrap_or(parent_span),
                },
            },
        }
    }
}

impl<T: FromTomlItem> FromTomlItem for Option<T> {
    fn from_toml_item(
        item: &toml_edit::Item,
        parent_span: Range<usize>,
        col: &TomlCollector,
    ) -> TomlValue<Self> {
        let res: TomlValue<T> = FromTomlItem::from_toml_item(item, parent_span, col);
        TomlValue {
            value: Some(res.value),
            state: res.state,
        }
    }
}

impl<T: FromTomlItem> FromTomlItem for Box<T> {
    fn from_toml_item(
        item: &toml_edit::Item,
        parent_span: Range<usize>,
        col: &TomlCollector,
    ) -> TomlValue<Self> {
        let res: TomlValue<T> = FromTomlItem::from_toml_item(item, parent_span, col);
        TomlValue {
            value: res.value.map(Box::new),
            state: res.state,
        }
    }
}

// Blanket implementation for Vec<T> where T: FromTomlItem
// This handles both regular arrays and ArrayOfTables (for Vec<toml_edit::Item> specifically)
#[allow(clippy::too_many_lines)]
impl<T: FromTomlItem> FromTomlItem for Vec<TomlValue<T>> {
    fn from_toml_item(
        item: &toml_edit::Item,
        parent_span: Range<usize>,
        col: &TomlCollector,
    ) -> TomlValue<Self> {
        match item {
            toml_edit::Item::None => unreachable!(),
            toml_edit::Item::Value(toml_edit::Value::Array(array)) => {
                let mut values = Self::with_capacity(array.len());
                let mut has_error = false;

                for array_item in array {
                    let item_span = array_item.span().unwrap_or(parent_span.clone());
                    let wrapped_item = toml_edit::Item::Value(array_item.clone());
                    let element: TomlValue<T> =
                        FromTomlItem::from_toml_item(&wrapped_item, item_span.clone(), col);

                    if !element.is_ok() {
                        has_error = true;
                    }
                    values.push(element);
                }

                if has_error {
                    TomlValue {
                        value: Some(values),
                        state: TomlValueState::Nested {},
                    }
                } else {
                    TomlValue {
                        value: Some(values),
                        state: TomlValueState::Ok {
                            span: array.span().unwrap_or(parent_span),
                        },
                    }
                }
            }
            // Handle ArrayOfTables - convert each table to an Item and deserialize
            toml_edit::Item::ArrayOfTables(array) => {
                let mut values = Self::with_capacity(array.len());
                let mut has_error = false;

                for table in array {
                    let table_span = table.span().unwrap_or(parent_span.clone());
                    let wrapped_item = toml_edit::Item::Table(table.clone());
                    let element: TomlValue<T> =
                        FromTomlItem::from_toml_item(&wrapped_item, table_span.clone(), col);

                    if !element.is_ok() {
                        has_error = true;
                    }
                    values.push(element);
                }

                if has_error {
                    TomlValue {
                        value: Some(values),
                        state: TomlValueState::Nested {},
                    }
                } else {
                    TomlValue {
                        value: Some(values),
                        state: TomlValueState::Ok {
                            span: array.span().unwrap_or(parent_span),
                        },
                    }
                }
            }
            toml_edit::Item::Value(value) => match col.vec_mode {
                VecMode::SingleOk => {
                    let element: TomlValue<T> =
                        FromTomlItem::from_toml_item(item, parent_span, col);
                    TomlValue {
                        state: if element.is_ok() {
                            TomlValueState::Ok {
                                span: element.span(),
                            }
                        } else {
                            TomlValueState::Nested {}
                        },
                        value: Some(vec![element]),
                    }
                }
                VecMode::Strict => TomlValue {
                    value: None,
                    state: TomlValueState::WrongType {
                        span: value.span().unwrap_or(parent_span),
                        expected: "array",
                        found: value.type_name(),
                    },
                },
            },
            toml_edit::Item::Table(value) => match col.vec_mode {
                VecMode::SingleOk => {
                    let element: TomlValue<T> =
                        FromTomlItem::from_toml_item(item, parent_span, col);
                    TomlValue {
                        state: if element.is_ok() {
                            TomlValueState::Ok {
                                span: element.span(),
                            }
                        } else {
                            TomlValueState::Nested {}
                        },
                        value: Some(vec![element]),
                    }
                }
                VecMode::Strict => TomlValue {
                    value: None,
                    state: TomlValueState::WrongType {
                        span: value.span().unwrap_or(parent_span),
                        expected: "array",
                        found: "table",
                    },
                },
            },
        }
    }
}

#[allow(clippy::too_many_lines)]
impl<S, T> FromTomlItem for IndexMap<S, TomlValue<T>>
where
    S: From<String> + std::hash::Hash + Eq,
    T: FromTomlItem,
{
    fn from_toml_item(
        item: &toml_edit::Item,
        parent_span: Range<usize>,
        col: &TomlCollector,
    ) -> TomlValue<Self> {
        match item {
            toml_edit::Item::None => unreachable!(),
            toml_edit::Item::Value(toml_edit::Value::InlineTable(inline_table)) => {
                let mut map = IndexMap::new();
                let mut has_errors = false;

                for (key, value) in inline_table.iter() {
                    let item_span = value.span().unwrap_or(parent_span.clone());
                    let wrapped_item = toml_edit::Item::Value(value.clone());
                    let val: TomlValue<T> =
                        FromTomlItem::from_toml_item(&wrapped_item, item_span.clone(), col);

                    if !val.is_ok() {
                        has_errors = true;
                    }
                    map.insert(S::from(key.to_string()), val);
                }

                if has_errors {
                    TomlValue {
                        value: Some(map),
                        state: TomlValueState::Nested {},
                    }
                } else {
                    TomlValue {
                        value: Some(map),
                        state: TomlValueState::Ok {
                            span: inline_table.span().unwrap_or(parent_span),
                        },
                    }
                }
            }
            toml_edit::Item::Table(table) => {
                let mut map = IndexMap::new();
                let mut has_errors = false;

                for (key, value) in table.iter() {
                    let item_span = value.span().unwrap_or(parent_span.clone());
                    let val: TomlValue<T> =
                        FromTomlItem::from_toml_item(value, item_span.clone(), col);

                    if !val.is_ok() {
                        has_errors = true;
                    }
                    map.insert(S::from(key.to_string()), val);
                }

                if has_errors {
                    TomlValue {
                        value: Some(map),
                        state: TomlValueState::Nested {},
                    }
                } else {
                    TomlValue {
                        value: Some(map),
                        state: TomlValueState::Ok {
                            span: table.span().unwrap_or(parent_span),
                        },
                    }
                }
            }
            toml_edit::Item::Value(value) => TomlValue {
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span),
                    expected: "table or inline table",
                    found: value.type_name(),
                },
            },
            toml_edit::Item::ArrayOfTables(array) => TomlValue {
                value: None,
                state: TomlValueState::WrongType {
                    span: array.span().unwrap_or(parent_span),
                    expected: "table or inline table",
                    found: "array of tables",
                },
            },
        }
    }
}
