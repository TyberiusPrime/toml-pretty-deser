use std::collections::HashSet;
use std::fmt::Display;
use std::{cell::RefCell, ops::Range, rc::Rc};
use toml_edit::{Document, TomlError};

pub use toml_pretty_deser_macros::make_partial;

pub fn deserialize<P, T>(source: &str) -> Result<T, DeserError<P>>
where
    P: FromTomlTable<()> + ToConcrete<T>,
{
    let parsed_toml = source.parse::<Document<String>>()?;
    let source = Rc::new(RefCell::new(source.to_string()));

    let errors = Rc::new(RefCell::new(Vec::new()));
    let mut helper = TomlHelper::new(parsed_toml.as_table(), errors.clone());

    let partial = P::from_toml_table(&mut helper, &());
    helper.deny_unknown();

    partial.collect_errors(&errors);
    if !errors.borrow().is_empty() {
        return Err(DeserError::DeserFailure(
            helper.into_inner(&source),
            partial,
        ));
    }

    if partial.can_concrete() {
        Ok(partial
            .to_concrete()
            .expect("can_concrete() returned true; qed"))
    } else {
        Err(DeserError::StillIncomplete(
            helper.into_inner(&source),
            partial,
        ))
    }
}

fn suggest_alternatives<T: AsRef<str>>(current: &str, available: &[T]) -> String {
    if current.is_empty() {
        let mut sorted: Vec<&str> = available.iter().map(AsRef::as_ref).collect::<Vec<&str>>();
        sorted.sort_unstable();
        return format!("Available are: {}", format_quoted_list(&sorted));
    }

    let mut distances: Vec<(usize, &str)> = available
        .iter()
        .map(|item| {
            let item_str = item.as_ref();
            let dist = strsim::levenshtein(current, item_str);
            (dist, item_str)
        })
        .collect();

    distances.sort_by_key(|k| k.0);

    let closest: Vec<&str> = distances.into_iter().take(3).map(|(_, s)| s).collect();

    if closest.is_empty() {
        "All known keys have been used.".to_string()
    } else {
        format!("Did you mean: {}?", format_quoted_list(&closest))
    }
}

fn format_quoted_list(items: &[&str]) -> String {
    match items {
        [] => String::new(),
        [single] => format!("'{}'", single),
        [first, second] => format!("'{}' or '{}'", first, second),
        rest => {
            let (last, init) = rest.split_last().expect("can't fail");
            let start = init
                .iter()
                .map(|s| format!("'{}'", s))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}, or '{}'", start, last)
        }
    }
}

#[derive(Debug)]
pub enum DeserError<P> {
    ParsingFailure(TomlError),
    DeserFailure(Vec<HydratedAnnotatedError>, P),
    StillIncomplete(Vec<HydratedAnnotatedError>, P),
}

impl<P> Display for DeserError<P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeserError::ParsingFailure(e) => write!(f, "TOML parsing error: {}", e),
            DeserError::DeserFailure(errors, _) => {
                write!(f, "Deserialization failed with {} errors", errors.len())
            }
            DeserError::StillIncomplete(errors, _) => {
                write!(f, "Incomplete deserialization with {} errors", errors.len())
            }
        }
    }
}

impl<P: std::fmt::Debug> std::error::Error for DeserError<P> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            DeserError::ParsingFailure(e) => Some(e),
            _ => None,
        }
    }
}

impl<P> From<TomlError> for DeserError<P> {
    fn from(value: TomlError) -> Self {
        DeserError::ParsingFailure(value)
    }
}

pub trait FromTomlTable<T> {
    fn from_toml_table(helper: &mut TomlHelper<'_>, partial: &T) -> Self
    where
        Self: Sized;
}

pub trait ToConcrete<T> {
    fn collect_errors(&self, errors: &Rc<RefCell<Vec<AnnotatedError>>>);
    fn can_concrete(&self) -> bool;
    fn to_concrete(self) -> Option<T>;
}

#[derive(Debug, Clone)]
pub struct AnnotatedError {
    pub spans: Vec<SpannedMessage>,
    pub help: Option<String>,
}

#[derive(Debug, Clone)]
pub struct SpannedMessage {
    pub span: Range<usize>,
    pub msg: String,
}

#[derive(Debug)]
pub struct HydratedAnnotatedError {
    pub source: Rc<RefCell<String>>,
    pub inner: AnnotatedError,
}

impl AnnotatedError {
    pub fn unplaced(help: &str) -> Self {
        AnnotatedError {
            spans: vec![],
            help: Some(help.to_string()),
        }
    }

    pub fn placed(span: Range<usize>, msg: &str, help: &str) -> Self {
        AnnotatedError {
            spans: vec![SpannedMessage {
                span,
                msg: msg.to_string(),
            }],
            help: Some(help.to_string()),
        }
    }
}

pub trait AnnotatedErrorExt {
    fn add_span(&mut self, span: Range<usize>, msg: &str);
}

impl AnnotatedErrorExt for AnnotatedError {
    fn add_span(&mut self, span: Range<usize>, msg: &str) {
        self.spans.push(SpannedMessage {
            span,
            msg: msg.to_string(),
        });
    }
}

impl HydratedAnnotatedError {
    pub fn pretty(&self, source_name: &str) -> String {
        use bstr::{BStr, ByteSlice};
        use codesnake::{Block, CodeWidth, Label, LineIndex};
        use std::fmt::Write;

        let source = self.source.borrow();

        if !self.inner.spans.is_empty() {
            let idx = LineIndex::new(&source);
            let mut spans = self.inner.spans.clone();
            spans.sort_by_key(|span| span.span.start);

            let previous_newline =
                memchr::memmem::rfind(&source.as_bytes()[..spans[0].span.start], b"\n");
            let mut labels = Vec::new();

            for span in spans.into_iter() {
                labels.push(Label::new(span.span).with_text(span.msg));
            }

            let block = Block::new(&idx, labels).unwrap_or_else(|| {
                let mut spans = self.inner.spans.clone();
                spans.sort_by_key(|span| span.span.start);
                let span_str: Vec<_> = spans
                    .iter()
                    .map(|span| format!("{}..{}: {}", span.span.start, span.span.end, span.msg))
                    .collect();
                let span_str = span_str.join("\n");
                let final_message = format!(
                    "Error spans were overlapping so we were unable to process a pretty code block. Spans & messages:\n{}",
                    span_str
                );
                let labels = vec![Label::new(0..0).with_text(final_message)];
                Block::new(&idx, labels).expect("can not fail")
            });

            let (lines_before, digits_needed) = match previous_newline {
                None => ("".to_string(), 1),
                Some(previous_newline) => {
                    let upto_span = &BStr::new(source.as_bytes())[..previous_newline];
                    let lines: Vec<_> = upto_span.lines().collect();
                    let str_line_no = format!("{}", lines.len());
                    let digits_needed = str_line_no.len();
                    let mut seen_opening = false;
                    let mut lines_before: Vec<_> = lines
                        .into_iter()
                        .enumerate()
                        .map(|(line_no, line)| (line_no, line))
                        .rev()
                        .take_while(move |x| {
                            if BStr::new(x.1).trim_ascii_start().starts_with(b"[") {
                                seen_opening = true;
                                true
                            } else {
                                !seen_opening
                            }
                        })
                        .map(|(line_no, line)| {
                            format!(
                                "{:>digits_needed$} │ {}",
                                line_no + 1,
                                std::string::String::from_utf8_lossy(line)
                            )
                        })
                        .collect();
                    lines_before.reverse();
                    (lines_before.join("\n"), digits_needed)
                }
            };

            let block = block.map_code(|c| CodeWidth::new(c, c.len()));
            let mut out = String::new();
            writeln!(&mut out, "{}{}", block.prologue(), source_name).expect("can't fail");
            write!(&mut out, " {:digits_needed$}┆\n{}\n", " ", lines_before).expect("can't fail");
            let blockf: String = format!("{}", block)
                .lines()
                .skip(1)
                .map(|x| format!("{}\n", x))
                .collect();
            write!(&mut out, "{}", blockf).expect("can't fail");
            writeln!(&mut out, "{}", block.epilogue()).expect("can't fail");

            if let Some(help) = self.inner.help.as_ref() {
                if !help.is_empty() {
                    let mut first = true;
                    write!(&mut out, "Hint: ").expect("Can't fail");
                    for line in help.lines() {
                        if !first {
                            write!(&mut out, "      ").expect("can't fail");
                        }
                        first = false;
                        writeln!(&mut out, "{}", line).expect("can't fail");
                    }
                }
            }
            out
        } else {
            format!(
                "ConfigError at unknown location. Help text: {}",
                self.inner
                    .help
                    .as_ref()
                    .map(|x| x.as_str())
                    .unwrap_or("None available")
            )
        }
    }
}

pub struct TomlHelper<'a> {
    table: &'a toml_edit::Table,
    expected: Vec<String>,
    allowed: Vec<String>,
    pub(crate) errors: Rc<RefCell<Vec<AnnotatedError>>>,
}

impl<'a> TomlHelper<'a> {
    pub fn new(table: &'a toml_edit::Table, errors: Rc<RefCell<Vec<AnnotatedError>>>) -> Self {
        Self {
            table,
            expected: vec![],
            allowed: vec![],
            errors,
        }
    }

    pub fn into_inner(self, source: &Rc<RefCell<String>>) -> Vec<HydratedAnnotatedError> {
        self.errors
            .borrow_mut()
            .drain(..)
            .map(|error| HydratedAnnotatedError {
                source: source.clone(),
                inner: error,
            })
            .collect()
    }

    pub fn get<T>(&mut self, key: &str) -> TomlValue<T>
    where
        TomlValue<T>: FromTomlItem,
    {
        let parent_span = self.table.span().unwrap_or(0..0);
        self.expected.push(key.to_string());

        match self.table.get(key) {
            Some(item) => {
                let res: TomlValue<T> = FromTomlItem::from_toml_item(item, parent_span);
                match &res.state {
                    TomlValueState::NotSet => unreachable!(),
                    TomlValueState::Missing { .. } => {}
                    _ => {
                        self.allowed.push(key.to_string());
                    }
                }
                res
            }
            None => {
                // For missing keys, call from_toml_item with Item::None
                // so that optional types can handle it appropriately
                let key_str = key.to_string();
                let mut res: TomlValue<T> =
                    FromTomlItem::from_toml_item(&toml_edit::Item::None, parent_span);
                // Update the key in the Missing state if needed
                if let TomlValueState::Missing { ref mut key, .. } = res.state {
                    if key.is_empty() {
                        *key = key_str;
                    }
                }
                res
            }
        }
    }

    pub fn add_err(&self, err: AnnotatedError) {
        self.errors.borrow_mut().push(err);
    }

    pub fn add_err_by_key(&self, key: &str, msg: &str, help: &str) {
        let span = self
            .table
            .key(key)
            .and_then(|item| item.span())
            .unwrap_or(0..0);
        self.add_err_by_span(span.clone(), msg, help);
    }

    pub fn add_err_by_span(&self, span: Range<usize>, msg: &str, help: &str) {
        self.errors
            .borrow_mut()
            .push(AnnotatedError::placed(span.clone(), msg, help));
    }

    pub fn deny_unknown(&self) {
        let expected_set: HashSet<String> = self.expected.iter().cloned().collect();

        for (key, _) in self.table.iter() {
            if !self
                .allowed
                .iter()
                .any(|allowed| allowed.eq_ignore_ascii_case(key))
            {
                let still_available: Vec<_> = expected_set
                    .iter()
                    .filter(|expected| {
                        !self
                            .allowed
                            .iter()
                            .any(|allowed| allowed.eq_ignore_ascii_case(expected))
                    })
                    .map(|s| s.as_str())
                    .collect();

                self.add_err_by_key(
                    key,
                    &format!("Unknown key: {}", key),
                    &suggest_alternatives(key, &still_available),
                );
            }
        }
    }
}

pub trait FromTomlItem {
    fn is_optional() -> bool;
    fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self;
}

macro_rules! impl_from_toml_item_integer {
    ($ty:ty, $name:expr) => {
        impl FromTomlItem for TomlValue<$ty> {
            fn is_optional() -> bool {
                true
            }

            fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self {
                match item {
                    toml_edit::Item::None => TomlValue {
                        value: None,
                        required: true,
                        state: TomlValueState::Missing {
                            key: "".to_string(),
                            parent_span,
                        },
                    },
                    toml_edit::Item::Value(toml_edit::Value::Integer(formatted)) => {
                        let value_i64 = *formatted.value();
                        if value_i64 < <$ty>::MIN as i64 || value_i64 > <$ty>::MAX as i64 {
                            TomlValue {
                                required: true,
                                value: None,
                                state: TomlValueState::ValidationFailed {
                                    span: formatted.span().unwrap_or(parent_span.clone()),
                                    message: format!(
                                        "integer out of range. Accepted: {}..{}",
                                        <$ty>::MIN,
                                        <$ty>::MAX
                                    ),
                                },
                            }
                        } else {
                            TomlValue {
                                required: true,
                                value: Some(value_i64 as $ty),
                                state: TomlValueState::Ok {
                                    span: formatted.span().unwrap_or(parent_span.clone()),
                                },
                            }
                        }
                    }
                    toml_edit::Item::Value(value) => TomlValue {
                        required: true,
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: $name,
                            found: value.type_name(),
                        },
                    },
                    toml_edit::Item::Table(value) => TomlValue {
                        required: true,
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: $name,
                            found: "table",
                        },
                    },
                    toml_edit::Item::ArrayOfTables(value) => TomlValue {
                        required: true,
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
impl_from_toml_item_integer!(i16, "i16");
impl_from_toml_item_integer!(i32, "i32");
impl_from_toml_item_integer!(u32, "u32");
impl_from_toml_item_integer!(i64, "i64");
impl_from_toml_item_integer!(u64, "u64");

impl FromTomlItem for TomlValue<f64> {
    fn is_optional() -> bool {
        true
    }

    fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self {
        match item {
            toml_edit::Item::None => TomlValue {
                value: None,
                required: true,
                state: TomlValueState::Missing {
                    key: "".to_string(),
                    parent_span,
                },
            },
            toml_edit::Item::Value(toml_edit::Value::Float(formatted)) => {
                let value = *formatted.value();
                TomlValue {
                    required: true,
                    value: Some(value),
                    state: TomlValueState::Ok {
                        span: formatted.span().unwrap_or(parent_span.clone()),
                    },
                }
            }
            toml_edit::Item::Value(toml_edit::Value::Integer(formatted)) => {
                let value = *formatted.value() as f64;
                TomlValue {
                    required: true,
                    value: Some(value),
                    state: TomlValueState::Ok {
                        span: formatted.span().unwrap_or(parent_span.clone()),
                    },
                }
            }
            toml_edit::Item::Value(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "f64",
                    found: value.type_name(),
                },
            },
            toml_edit::Item::Table(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "f64",
                    found: "table",
                },
            },
            toml_edit::Item::ArrayOfTables(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "f64",
                    found: "array of tables",
                },
            },
        }
    }
}

impl FromTomlItem for TomlValue<bool> {
    fn is_optional() -> bool {
        true
    }

    fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self {
        match item {
            toml_edit::Item::None => TomlValue {
                value: None,
                required: true,
                state: TomlValueState::Missing {
                    key: "".to_string(),
                    parent_span,
                },
            },
            toml_edit::Item::Value(toml_edit::Value::Boolean(formatted)) => {
                let value = *formatted.value();
                TomlValue {
                    required: true,
                    value: Some(value),
                    state: TomlValueState::Ok {
                        span: formatted.span().unwrap_or(parent_span.clone()),
                    },
                }
            }
            toml_edit::Item::Value(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "bool",
                    found: value.type_name(),
                },
            },
            toml_edit::Item::Table(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "bool",
                    found: "table",
                },
            },
            toml_edit::Item::ArrayOfTables(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "bool",
                    found: "array of tables",
                },
            },
        }
    }
}

impl FromTomlItem for TomlValue<String> {
    fn is_optional() -> bool {
        true
    }

    fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self {
        match item {
            toml_edit::Item::None => TomlValue {
                value: None,
                required: true,
                state: TomlValueState::Missing {
                    key: "".to_string(),
                    parent_span,
                },
            },
            toml_edit::Item::Value(toml_edit::Value::String(formatted)) => {
                let value = formatted.value().to_string();
                TomlValue {
                    required: true,
                    value: Some(value),
                    state: TomlValueState::Ok {
                        span: formatted.span().unwrap_or(parent_span.clone()),
                    },
                }
            }
            toml_edit::Item::Value(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "string",
                    found: value.type_name(),
                },
            },
            toml_edit::Item::Table(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "string",
                    found: "table",
                },
            },
            toml_edit::Item::ArrayOfTables(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "string",
                    found: "array of tables",
                },
            },
        }
    }
}

macro_rules! impl_from_toml_item_option {
    ($ty:ty) => {
        impl FromTomlItem for TomlValue<Option<$ty>> {
            fn is_optional() -> bool {
                false
            }

            fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self {
                let mut res: TomlValue<$ty> = FromTomlItem::from_toml_item(item, parent_span);
                res.required = false;
                match res.state {
                    TomlValueState::Ok { span } => TomlValue {
                        required: false,
                        value: Some(res.value),
                        state: TomlValueState::Ok { span },
                    },
                    TomlValueState::Missing { .. } => TomlValue {
                        required: false,
                        value: Some(None),
                        state: TomlValueState::Ok { span: 0..0 },
                    },
                    _ => TomlValue {
                        value: None,
                        required: false,
                        state: res.state,
                    },
                }
            }
        }
    };
}

impl_from_toml_item_option!(u8);
impl_from_toml_item_option!(i16);
impl_from_toml_item_option!(i32);
impl_from_toml_item_option!(i64);
impl_from_toml_item_option!(f64);
impl_from_toml_item_option!(bool);
impl_from_toml_item_option!(String);

macro_rules! impl_from_toml_item_vec {
    ($ty:ty, $name:expr) => {
        impl FromTomlItem for TomlValue<Vec<$ty>> {
            fn is_optional() -> bool {
                true
            }

            fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self {
                match item {
                    toml_edit::Item::None => TomlValue {
                        value: None,
                        required: true,
                        state: TomlValueState::Missing {
                            key: "".to_string(),
                            parent_span,
                        },
                    },
                    toml_edit::Item::Value(toml_edit::Value::Array(array)) => {
                        let mut values = Vec::with_capacity(array.len());
                        let mut has_error = false;

                        for item in array.iter() {
                            let item_span = item.span().unwrap_or(parent_span.clone());
                            let wrapped_item = toml_edit::Item::Value(item.clone());
                            let element: TomlValue<$ty> =
                                FromTomlItem::from_toml_item(&wrapped_item, item_span.clone());

                            match &element.state {
                                TomlValueState::Ok { .. } => {
                                    if let Some(val) = element.value {
                                        values.push(val);
                                    }
                                }
                                _ => {
                                    has_error = true;
                                }
                            }
                        }

                        if has_error {
                            TomlValue {
                                required: true,
                                value: None,
                                state: TomlValueState::ValidationFailed {
                                    span: array.span().unwrap_or(parent_span.clone()),
                                    message: "Array contains invalid elements".to_string(),
                                },
                            }
                        } else {
                            TomlValue {
                                required: true,
                                value: Some(values),
                                state: TomlValueState::Ok {
                                    span: array.span().unwrap_or(parent_span.clone()),
                                },
                            }
                        }
                    }
                    toml_edit::Item::Value(value) => TomlValue {
                        required: true,
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: "array",
                            found: value.type_name(),
                        },
                    },
                    toml_edit::Item::Table(value) => TomlValue {
                        required: true,
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: "array",
                            found: "table",
                        },
                    },
                    toml_edit::Item::ArrayOfTables(value) => TomlValue {
                        required: true,
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: "array",
                            found: "array of tables",
                        },
                    },
                }
            }
        }

        impl FromTomlItem for TomlValue<Option<Vec<$ty>>> {
            fn is_optional() -> bool {
                false
            }

            fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self {
                let mut res: TomlValue<Vec<$ty>> = FromTomlItem::from_toml_item(item, parent_span);
                res.required = false;
                match res.state {
                    TomlValueState::Ok { span } => TomlValue {
                        required: false,
                        value: Some(res.value),
                        state: TomlValueState::Ok { span },
                    },
                    TomlValueState::Missing { .. } => TomlValue {
                        required: false,
                        value: Some(None),
                        state: TomlValueState::Ok { span: 0..0 },
                    },
                    _ => TomlValue {
                        value: None,
                        required: false,
                        state: res.state,
                    },
                }
            }
        }
    };
}

impl_from_toml_item_vec!(u8, "u8");
impl_from_toml_item_vec!(i16, "i16");
impl_from_toml_item_vec!(i32, "i32");
impl_from_toml_item_vec!(i64, "i64");
impl_from_toml_item_vec!(f64, "f64");
impl_from_toml_item_vec!(bool, "bool");
impl_from_toml_item_vec!(String, "string");

#[derive(Debug, Clone)]
pub struct TomlValue<T> {
    pub(crate) value: Option<T>,
    pub(crate) state: TomlValueState,
    pub(crate) required: bool,
}

impl<T> TomlValue<T> {
    pub fn has_value(&self) -> bool {
        matches!(self.state, TomlValueState::Ok { .. })
    }

    pub fn into_option(self) -> Option<T> {
        match self.state {
            TomlValueState::Ok { .. } => self.value,
            _ => None,
        }
    }

    pub fn unwrap(self) -> T {
        match self.state {
            TomlValueState::Ok { .. } => self.value.unwrap(),
            _ => panic!("Called unwrap on a TomlValue that is not Ok"),
        }
    }

    pub fn register_error(&self, errors: &Rc<RefCell<Vec<AnnotatedError>>>) {
        match &self.state {
            TomlValueState::NotSet => {}
            TomlValueState::Missing { key, parent_span } => {
                if self.required {
                    errors.borrow_mut().push(AnnotatedError::placed(
                        parent_span.clone(),
                        &format!("Missing required key: {}", key),
                        "This key is required but was not found in the TOML document.",
                    ));
                }
            }
            TomlValueState::WrongType {
                span,
                expected,
                found,
            } => {
                errors.borrow_mut().push(AnnotatedError::placed(
                    span.clone(),
                    &format!("Wrong type: expected {}, found {}", expected, found),
                    "The value has the wrong type.",
                ));
            }
            TomlValueState::ValidationFailed { span, message } => {
                errors.borrow_mut().push(AnnotatedError::placed(
                    span.clone(),
                    &format!("Validation failed: {}", message),
                    "The value failed validation checks.",
                ));
            }
            TomlValueState::Ok { .. } => {}
        }
    }

    pub fn verify<F>(self, verification_func: F) -> TomlValue<T>
    where
        F: FnOnce(&T) -> Result<(), String>,
    {
        match &self.state {
            TomlValueState::Ok { span } => match verification_func(self.value.as_ref().unwrap()) {
                Ok(()) => self,
                Err(msg) => TomlValue {
                    value: None,
                    required: self.required,
                    state: TomlValueState::ValidationFailed {
                        span: span.clone(),
                        message: msg,
                    },
                },
            },
            _ => self,
        }
    }

    pub fn or_default(self, default: T) -> TomlValue<T> {
        match &self.state {
            TomlValueState::Missing { .. } => TomlValue {
                value: Some(default),
                required: false,
                state: TomlValueState::Ok { span: 0..0 },
            },
            _ => self,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TomlValueState {
    NotSet,
    Missing {
        key: String,
        parent_span: Range<usize>,
    },
    WrongType {
        span: Range<usize>,
        expected: &'static str,
        found: &'static str,
    },
    ValidationFailed {
        span: Range<usize>,
        message: String,
    },
    Ok {
        span: Range<usize>,
    },
}
