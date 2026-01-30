use std::collections::HashSet;
use std::fmt::Display;
use std::{cell::RefCell, ops::Range, rc::Rc};
use toml_edit::{Document, TomlError};

//main entry point
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
        // Extract errors and clone partial before consuming it
        let errors = helper.into_inner(&source);
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

/// Helper for suggesting alternative keys when a key is not found.
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

/// Main error type for deserialization failures.
#[derive(Debug)]
pub enum DeserError<P> {
    /// TOML parsing failed (syntax error)
    ParsingFailure(TomlError),

    /// Deserialization failed with errors collected during parsing
    DeserFailure(Vec<HydratedAnnotatedError>, P),

    /// Some required fields are still missing
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

/// Trait for types that can be deserialized from a TOML table.
///
///A these get derived from a make_partial proc
pub trait FromTomlTable<T> {
    /// Deserialize from a TOML table.
    ///
    /// The `partial` parameter allows nested structs to access parent data
    /// for computed fields.
    fn from_toml_table(helper: &mut TomlHelper<'_>, partial: &T) -> Self
    where
        Self: Sized;
}

/// Trait for converting a Partial struct to its concrete form.
///
/// The Partial struct holds `TomlValue<T>` fields which may be in various
/// error states. This trait checks if all required fields are `Ok` and
/// performs the conversion.
pub trait ToConcrete<T> {
    /// Check if all required fields are present and valid.
    ///
    /// Returns true only if all `TomlValue` fields are in the `Ok` state.
    fn collect_errors(&self, errors: &Rc<RefCell<Vec<AnnotatedError>>>);
    fn can_concrete(&self) -> bool;

    /// Convert to the concrete type.
    ///
    /// Returns `Some(T)` if all required fields are valid, `None` otherwise.
    /// This should only be called after `can_concrete()` returns true.
    fn to_concrete(self) -> Option<T>;
}

impl ToConcrete<Output> for PartialOutput {
    fn collect_errors(&self, errors: &Rc<RefCell<Vec<AnnotatedError>>>) {
        self.a_u8.register_error(errors);
        self.a_i64.register_error(errors);
    }

    fn can_concrete(&self) -> bool {
        self.a_u8.has_value() && self.a_i64.has_value()
        // && self.a_f64.has_value()
        // && self.a_string.has_value()
        // && self.a_bool.has_value()
        // && self.verified_i16.has_value()
        // && self.defaulted_i16.has_value()
    }

    fn to_concrete(self) -> Option<Output> {
        Some(Output {
            a_u8: self.a_u8.unwrap(),
            a_i64: self.a_i64.unwrap(),
            // a_f64: self.a_f64.unwrap(),
            // a_string: self.a_string.unwrap(),
            // a_bool: self.a_bool.unwrap(),
            opt_a_u8: self.opt_a_u8.unwrap(),
            opt_a_i64: self.opt_a_i64.unwrap(),
            // opt_a_f64: self.opt_a_f64.unwrap(),
            // opt_a_string: self.opt_a_string.unwrap(),
            // opt_a_bool: self.opt_a_bool.unwrap(),
            // verified_i16: self.verified_i16.unwrap(),
            // defaulted_i16: self.defaulted_i16.unwrap(),
        })
    }
}

/// An error with location information in the source text.
#[derive(Debug, Clone)]
pub struct AnnotatedError {
    pub spans: Vec<SpannedMessage>,
    pub help: Option<String>,
}

/// A message associated with a specific span in the source.
#[derive(Debug, Clone)]
pub struct SpannedMessage {
    pub span: Range<usize>,
    pub msg: String,
}

/// An annotated error with access to the source text for pretty printing.
#[derive(Debug)]
pub struct HydratedAnnotatedError {
    pub source: Rc<RefCell<String>>,
    pub inner: AnnotatedError,
}

impl AnnotatedError {
    /// Create an error without a specific location.
    pub fn unplaced(help: &str) -> Self {
        AnnotatedError {
            spans: vec![],
            help: Some(help.to_string()),
        }
    }

    /// Create an error at a specific location.
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

/// Extension trait for adding additional spans to errors.
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
    /// Generate a pretty-printed error message.
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

/// Main helper for parsing TOML tables.
pub struct TomlHelper<'a> {
    table: &'a toml_edit::Table,
    expected: Vec<String>,
    allowed: Vec<String>,
    pub(crate) errors: Rc<RefCell<Vec<AnnotatedError>>>,
}

impl<'a> TomlHelper<'a> {
    /// Create a new helper for the given table.
    pub fn new(table: &'a toml_edit::Table, errors: Rc<RefCell<Vec<AnnotatedError>>>) -> Self {
        Self {
            table,
            expected: vec![],
            allowed: vec![],
            errors,
        }
    }

    /// Convert errors into hydrated errors with source text.
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

    fn get<T>(&mut self, key: &str) -> TomlValue<T>
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
                    TomlValueState::Missing { .. } => {
                        //nothing
                    }
                    _ => {
                        self.allowed.push(key.to_string());
                    }
                }
                res
            }
            None => TomlValue {
                value: None,
                required: TomlValue::<T>::is_optional(),
                state: TomlValueState::Missing {
                    key: key.to_string(),
                    parent_span,
                },
            },
        }
    }
    /// Add an error to the collection.
    pub fn add_err(&self, err: AnnotatedError) {
        self.errors.borrow_mut().push(err);
    }

    /// Add an error by key.
    pub fn add_err_by_key(&self, key: &str, msg: &str, help: &str) {
        let span = self
            .table
            .key(key)
            .and_then(|item| item.span())
            .unwrap_or(0..0);
        self.add_err_by_span(span.clone(), msg, help);
    }

    /// Add an error by span.
    pub fn add_err_by_span(&self, span: Range<usize>, msg: &str, help: &str) {
        self.errors
            .borrow_mut()
            .push(AnnotatedError::placed(span.clone(), msg, help));
    }

    /// Report errors for unknown keys in the table.
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

trait FromTomlItem {
    fn is_optional() -> bool;
    fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self;
}

impl FromTomlItem for TomlValue<u8> {
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
                if value_i64 < u8::MIN as i64 || value_i64 > u8::MAX as i64 {
                    TomlValue {
                        required: true,
                        value: None,
                        state: TomlValueState::ValidationFailed {
                            span: formatted.span().unwrap_or(parent_span.clone()),
                            message: "integer out of range. Accepted: 0..255".to_string(),
                        },
                    }
                } else {
                    TomlValue {
                        required: true,
                        value: Some(value_i64 as u8),
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
                    expected: "u8",
                    found: value.type_name(),
                },
            },
            toml_edit::Item::Table(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "u8",
                    found: "table",
                },
            },
            toml_edit::Item::ArrayOfTables(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "u8",
                    found: "array of tables",
                },
            },
        }
    }
}

impl FromTomlItem for TomlValue<i64> {
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
                if value_i64 < i64::MIN as i64 || value_i64 > i64::MAX as i64 {
                    TomlValue {
                        value: None,
                        required: true,
                        state: TomlValueState::WrongType {
                            span: formatted.span().unwrap_or(parent_span.clone()),
                            expected: "u8",
                            found: "integer out of range. Accepted: 0..255",
                        },
                    }
                } else {
                    TomlValue {
                        required: true,
                        value: Some(value_i64),
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
                    expected: "u8",
                    found: value.type_name(),
                },
            },
            toml_edit::Item::Table(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "u8",
                    found: "table",
                },
            },
            toml_edit::Item::ArrayOfTables(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "u8",
                    found: "array of tables",
                },
            },
        }
    }
}

impl FromTomlItem for TomlValue<Option<u8>> {
    fn is_optional() -> bool {
        false
    }
    fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self {
        let mut res: TomlValue<u8> = FromTomlItem::from_toml_item(item, parent_span);
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

impl FromTomlItem for TomlValue<Option<i64>> {
    fn is_optional() -> bool {
        false
    }
    fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self {
        let mut res: TomlValue<i64> = FromTomlItem::from_toml_item(item, parent_span);
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

#[derive(Debug, Clone)]
pub struct TomlValue<T> {
    pub(crate) value: Option<T>,
    pub(crate) state: TomlValueState,
    pub(crate) required: bool,
}

impl<T> TomlValue<T> {
    fn has_value(&self) -> bool {
        match self.state {
            TomlValueState::Ok { .. } => true,
            _ => false,
        }
    }

    fn into_option(self) -> Option<T> {
        match self.state {
            TomlValueState::Ok { .. } => self.value,
            _ => None,
        }
    }

    fn unwrap(self) -> T {
        match self.state {
            TomlValueState::Ok { .. } => self.value.unwrap(),
            _ => panic!("Called unwrap on a TomlValue that is not Ok"),
        }
    }

    fn register_error(&self, errors: &Rc<RefCell<Vec<AnnotatedError>>>) {
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
}

/// The actual state of the value, separated from the error collection.
#[derive(Debug, Clone)]
pub enum TomlValueState {
    NotSet,
    /// The key was not found in the TOML document
    Missing {
        key: String,
        parent_span: Range<usize>,
    },
    /// The value had the wrong type (e.g., string instead of integer)
    WrongType {
        span: Range<usize>,
        expected: &'static str,
        found: &'static str,
    },
    /// The value failed validation (e.g., out of range)
    ValidationFailed {
        span: Range<usize>,
        message: String,
    },
    /// The value was successfully parsed and validated
    Ok {
        span: Range<usize>,
    },
}

//this get's implemented by the user

#[derive(Debug)]
struct Output {
    a_u8: u8,
    a_i64: i64,
    // a_f64: f64,
    // a_string: String,
    // a_bool: bool,
    //
    opt_a_u8: Option<u8>,
    opt_a_i64: Option<i64>,
    // opt_a_f64: Option<f64>,
    // opt_a_string: Option<String>,
    // opt_a_bool: Option<bool>,
    //
    // verified_i16: i16,
    // defaulted_i16: i16,
}

#[derive(Debug)]
struct PartialOutput {
    a_u8: TomlValue<u8>,
    a_i64: TomlValue<i64>,
    // a_f64: TomlValue<f64>,
    // a_string: TomlValue<String>,
    // a_bool: TomlValue<bool>,
    opt_a_u8: TomlValue<Option<u8>>,
    opt_a_i64: TomlValue<Option<i64>>,
    // opt_a_f64: TomlValue<Option<f64>>,
    // opt_a_string: TomlValue<Option<String>>,
    // opt_a_bool: TomlValue<Option<bool>>,

    // verified_i16: TomlValue<i16>,
    // defaulted_i16: TomlValue<i16>,
}
//
//
impl FromTomlTable<()> for PartialOutput {
    fn from_toml_table(helper: &mut TomlHelper<'_>, _partial: &()) -> Self {
        PartialOutput {
            a_u8: helper.get("a_u8"),
            a_i64: helper.get("a_i64"),
            // a_f64: helper.get("f64"),
            // a_string: helper.get("string"),
            // a_bool: helper.get("bool"),
            opt_a_u8: helper.get("opt_a_u8"),
            opt_a_i64: helper.get("opt_a_i64"),
            //  opt_a_f64: helper.get("opt_f64"),
            // opt_a_string: helper.get("opt_string"),
            // opt_a_bool: helper.get("opt_bool"),
            // verified_i16: helper
            //     .get("verified_i16")
            //     .verify(|v: &i16| *v > 5, "Too small"),
            // defaulted_i16: helper.get("defaulted_i16").or_default(42),
        }
    }
}

#[test]
fn test_happy_path() {
    let toml = "
            a_u8 = 255
            a_i64 = -123
            # a_f64 = 3.14
            # a_string = 'Hello, World!'
            # a_bool = true

            opt_a_u8 = 128
            opt_a_i64 = -456
            # opt_a_f64 = 2.71
            # opt_a_string = 'Optional String'
            # opt_a_bool = false
            #
            # verified_i16 = 10
            # defaulted_i16 = 100
        ";

    let result: Result<_, _> = deserialize::<PartialOutput, Output>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a_u8, 255);
        assert_eq!(output.a_i64, -123);
        // assert_eq!(output.a_f64, 3.14);
        // assert_eq!(output.a_string, "Hello, World!");
        // assert_eq!(output.a_bool, true);
        assert_eq!(output.opt_a_u8, Some(128));
        // assert_eq!(output.opt_a_i64, Some(-456));
        // assert_eq!(output.opt_a_f64, Some(2.71));
        // assert_eq!(output.opt_a_string, Some("Optional String".to_string()));
        // assert_eq!(output.opt_a_bool, Some(false));
        // assert_eq!(output.verified_i16, 10);
        // assert_eq!(output.defaulted_i16, 100);
    }
}
#[test]
fn test_missing() {
    let toml = "
            # a_u8 = 255
            a_i64 = -123
            # a_f64 = 3.14
            # a_string = 'Hello, World!'
            # a_bool = true

            opt_a_u8 = 128
            opt_a_i64 = -456
            # opt_a_f64 = 2.71
            # opt_a_string = 'Optional String'
            # opt_a_bool = false
            #
            # verified_i16 = 10
            # defaulted_i16 = 100
        ";

    let result: Result<_, _> = deserialize::<PartialOutput, Output>(toml);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, output)) = result {
        assert_eq!(output.a_u8.into_option(), None);
        assert_eq!(output.a_i64.into_option(), Some(-123));
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].inner.spans[0].msg , "Missing required key: a_u8");
    } else {
        panic!("wrong result")
    }
}
