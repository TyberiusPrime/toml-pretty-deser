//! Deserialize [TOML 1.1.0](https://toml.io/en/v1.1.0) with pretty error messages.
//!
//! ## Usage:
//!
//! Deserialize your configuration structs from TOML with beautiful error message,
//! and keeping whatever could be understood around in case of errors:
//!
//! ```toml
//!         a = 5
//!         b = 10
//!         c = 3
//! ```
//!
//! becomes
//!
//! ```text
//! Error 1/2
//!   ╭─example.toml
//!   ┆
//!
//! 2 │         a = 5
//!   ┆             ┬
//!   ┆             │
//!   ┆             ╰─ a+b+c must add up to 100. Sum was 18.
//! 3 │         b = 10
//!   ┆             ─┬
//!   ┆              │
//!   ┆              ╰─ See a
//! 4 │         c = 3
//!   ┆             ┬
//!   ┆             │
//!   ┆             ╰─ See a
//! ──╯
//! Hint: For example, set a = 33, b=66, c=0
//!
//! Error 2/2
//!   ╭─example.toml
//!   ┆
//! 1 │
//! 2 │         a = 5
//! 3 │         b = 10
//! 4 │         c = 3
//! 5 │         d = 100
//!   ┆             ─┬─
//!   ┆              │
//!   ┆              ╰── Must be below 50
//! ──╯
//! Hint: For demonstration purposes only`
//!
//! Using this client Rust code:
//!
//! ```rust
//! use toml_pretty_deser::prelude::*;
//! #[tpd_make_partial(false)]
//! struct ShowOffTwoValueErrors {
//!     a: i64,
//!     b: i64,
//!     c: i64,
//!     d: i64,
//! }
//!
//! impl VerifyFromToml for PartialShowOffTwoValueErrors {
//!     fn verify(mut self, helper: &mut TomlHelper<'_>) -> Self
//!     where
//!         Self: Sized,
//!     {
//!         if let Some(a) = self.a.value
//!             && let Some(b) = self.b.value
//!             && let Some(c) = self.c.value
//!         {
//!             let sum = a + b + c;
//!             if sum != 99 {
//!                 let spans = vec![
//!                     (
//!                         self.a.span(),
//!                         format!("a+b+c must add up to 100. Sum was {sum}."),
//!                     ),
//!                     (self.b.span(), "See a".to_string()),
//!                     (self.c.span(), "See a".to_string()),
//!                 ];
//!                 helper.add_err_by_spans(spans, "For example, set a = 33, b=66, c=0")
//!             }
//!         }
//!
//!         self.d = self.d.verify(helper, |value| {
//!             if *value < 50 {
//!                 Ok(())
//!             } else {
//!                Err((
//!                    "Must be below 50".to_string(),
//!                    Some("For demonstration purposes only".to_string())
//!                ))
//!             }
//!         });
//!
//!         self
//!     }
//! }
//! let toml = "
//!         a = 5
//!         b = 10
//!         c = 3
//!         d = 100
//!         ";
//!
//!     let result = deserialize::<PartialShowOffTwoValueErrors, ShowOffTwoValueErrors>(toml);
//!     if let Err(err) = result {
//!         println!("{}", err.pretty("example.toml"));
//!     }
//! ```
//!
//! ## How this works:
//!
//! `#[tpd_make_partial]` writes a `PartialT` for every struct T you apply it on,
//! and implementations to go from `toml_edit` types to your `PartialT`, as well
//! as a conversion to turn complete `PartialT` back into T.
//!
//! The `PartialT` consists of all the same fields, wrapped in `TomlValue` - which records the
//! deserialization state and where in the TOML document it was.
//!
//! `deserialize` will then give you an Ok(T) or an Err with a list of errors and the partial
//! state (If parsing succeeded. Otherwise you get just a parse error). See [`DeserError`]
//!
//!
//! The hydrated errors contain information about what went wrong, and can turn themselves into pretty error messages
//! as in the example above.
//!
//! ## Why not just serde
//!
//! Serde is great. If it works for you, good, use it!.
//!
//! But I want pretty error messages, more than one error reported at once,
//! pin-point accuracy in the errors and the ability to continue on with incomplete
//! configurations. Oh, and optional case and snake-case insensitivity.
//!
//!
//! ## Usage Variations
//!
//! ### No custom validation
//!
//! Use `tpd_make_partial(true)` or plain `tpd_make_partial`.
//!
//! `VerifyFromToml for PartialT` will be produced by the macro.
//!
//! ### Nested structs
//!
//! When you want to represent `NestedT` as `PartialNestedT` inside your struct T,
//! tag them with `#[tpd_nested]`
//!
//! Complete error checking is provided either way.
//!
//! ### Enums
//!
//!
//! For simple string-typed Enums without an inner payload, tag the enum declaration with
//! [`toml_pretty_deser_macros::tpd_make_enum`].
//!
//! For deserializing tagged Enums with a struct payload,
//! use [`toml_pretty_deser_macros::tpd_make_tagged_enum`] on the enum declaration,
//! and pass in the tag key name (and it's aliases).
//!
//! Example:
//!
//! ```rust
//! use toml_pretty_deser::prelude::*;
//! #[tpd_make_partial]
//! #[derive(Debug)]
//! struct InnerA {
//!     n: i32,
//!     o: u32,
//! }
//!
//! #[tpd_make_partial]
//! #[derive(Debug)]
//! struct InnerB {
//!     s: u32,
//!     t: u32,
//! }
//!
//! #[tpd_make_tagged_enum("kind", aliases = ["type"])]
//! #[derive(Debug)]
//! enum EitherOne {
//!     KindA(InnerA),
//!     KindB(InnerB),
//!}
//! ```
//!
//!
//!## Aliases
//!
//! You can supply alias names on any field using `[#tpd_alias(name1, name2]`.
//! Note the section below on casing for the common case of case- or code-case-insensitivity
//!
//!
//!## Casing
//!
//! By default, field names and enum variants are matched strictl.
//!
//! [`deserialize_with_mode`] allows you to change this to case-insensitive (upper/lower only) or
//! code-case-insensitive (allowing `snake_case`, camelCase, kebab-case, etc. to match each other).
//!
//!
//!## Single elements to Vecs
//!
//! By default, when a field is `Vec<T>`, a TOML list must be provided.
//! [`deserialize_with_mode`] allows you to change this to allow single elements to be treated as
//! one-element Vecs instead.
//!
//!
//!## Custom types
//!
//! To enable the deserialization of custom types, implement [`FromTomlItem`] for them.
//!```
//! use toml_pretty_deser::prelude::*;
//! struct DNA(String);
//! impl FromTomlItem for DNA {
//!     fn from_toml_item(
//!         item: &toml_edit::Item,
//!         parent_span: std::ops::Range<usize>,
//!         _col: &TomlCollector,
//!     ) -> TomlValue<DNA> {
//!         match item.as_str() {
//!             Some(s) => {
//!                 if s.chars()
//!                     .all(|c| matches!(c, 'a' | 'c' | 'g' | 't' | 'A' | 'C' | 'G' | 'T'))
//!                 {
//!                     TomlValue::new_ok(DNA(s.to_string()), parent_span)
//!                 } else {
//!                     TomlValue::new_validation_failed(
//!                         item.span().unwrap_or(parent_span),
//!                         "Invalid base".to_string(),
//!                         Some("Use only AGTC".to_string()),
//!                     )
//!                 }
//!             }
//!             None => TomlValue::new_wrong_type(item, parent_span, "String(DNA)"),
//!         }
//!     }
//! }
//! ```
//!

use indexmap::{IndexMap, IndexSet};
use std::fmt::Write;
use std::{cell::RefCell, ops::Range, rc::Rc};
use toml_edit::{Document, TomlError};

/// Import `toml_pretty_deser::prelude::`* to make use of our macros
pub mod prelude;
mod tablelike;
pub use tablelike::{AsTableLikePlus, TableLikePlus};
pub use toml_pretty_deser_macros::{tpd_make_enum, tpd_make_partial, tpd_make_tagged_enum};

/// Get enum variant names.
///
/// Implemented by [`toml_pretty_deser_macros::tpd_make_enum`]
pub trait StringNamedEnum: Sized + Clone {
    fn all_variant_names() -> &'static [&'static str];
    fn from_str(s: &str) -> Option<Self>;
}

/// Trait for tagged enums that carry their tag key and aliases.
/// This is automatically implemented by `#[tpd_make_tagged_enum("tag_key")]`.
#[doc(hidden)]
pub trait TaggedEnumMeta: Sized {
    /// The tag key used to identify the variant (e.g., "kind" or "type")
    const TAG_KEY: &'static str;
    /// Aliases for the tag key (e.g., if "type" is an alias for "kind")
    const TAG_ALIASES: &'static [&'static str];
    /// All variant names for error messages and matching
    fn all_variant_names() -> &'static [&'static str];
    /// Deserialize a specific variant by name from a TOML item
    fn deserialize_variant(
        variant_name: &str,
        item: &toml_edit::Item,
        col: &TomlCollector,
        fields_to_ignore: &[&str],
    ) -> Option<Self>;
}

/// Controls how field names are matched against TOML keys
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldMatchMode {
    /// Exact match only (field name must match exactly)
    Exact,
    /// Case insensitive match (case insensitive exact match only)
    UpperLower,
    /// Any case variant match (allows camelCase, `snake_case`, kebab-case, etc.)
    AnyCase,
}

impl FieldMatchMode {
    /// Normalize a name according to the match mode
    #[must_use]
    pub fn normalize(&self, name: &str) -> String {
        match self {
            Self::Exact => name.to_string(),
            Self::UpperLower => name.to_lowercase(),
            Self::AnyCase => normalize_to_no_case(name),
        }
    }

    /// Check if two names match under this mode
    #[must_use]
    pub fn matches(&self, a: &str, b: &str) -> bool {
        self.normalize(a) == self.normalize(b)
    }
}

/// Convert any case variant to `snake_case` for comparison
/// Supports: camelCase, `UpperCamelCase`, `snake_case`, kebab-case, `SHOUTY_SNAKE_CASE`, Train-Case
fn normalize_to_no_case(s: &str) -> String {
    s.chars()
        .filter_map(|c| match c {
            'A'..='Z' => Some(c.to_lowercase().next().expect("can't fail")),
            'a'..='z' | '0'..='9' => Some(c),
            _ => None,
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normalize_no_case() {
        assert_eq!(normalize_to_no_case("snake_case"), "snakecase");
        assert_eq!(normalize_to_no_case("camelCase"), "camelcase");
        assert_eq!(normalize_to_no_case("UpperCamelCase"), "uppercamelcase");
        assert_eq!(normalize_to_no_case("kebab-case"), "kebabcase");
        assert_eq!(normalize_to_no_case("SHOUTY_SNAKE_CASE"), "shoutysnakecase");
        assert_eq!(normalize_to_no_case("Train-Case"), "traincase");
        assert_eq!(
            normalize_to_no_case("mixed_Case-Variant"),
            "mixedcasevariant"
        );
        assert_eq!(normalize_to_no_case("already_snake"), "alreadysnake");
        assert_eq!(normalize_to_no_case("HTMLParser"), "htmlparser");
        assert_eq!(normalize_to_no_case("getHTTPResponse"), "gethttpresponse");
    }

    #[test]
    fn test_field_match_mode() {
        let exact = FieldMatchMode::Exact;
        assert!(exact.matches("field_name", "field_name"));
        assert!(!exact.matches("field_name", "Field_Name"));
        assert!(!exact.matches("field_name", "fieldName"));

        let upper_lower = FieldMatchMode::UpperLower;
        assert!(upper_lower.matches("field_name", "field_name"));
        assert!(upper_lower.matches("field_name", "FIELD_NAME"));
        assert!(upper_lower.matches("field_name", "Field_Name"));
        assert!(!upper_lower.matches("field_name", "fieldName"));

        let any_case = FieldMatchMode::AnyCase;
        assert!(any_case.matches("field_name", "field_name"));
        assert!(any_case.matches("field_name", "fieldName"));
        assert!(any_case.matches("field_name", "FieldName"));
        assert!(any_case.matches("field_name", "field-name"));
        assert!(any_case.matches("field_name", "FIELD_NAME"));
        assert!(any_case.matches("field_name", "Field-Name"));
    }
}

/// Convenience entry point
///
/// See [`deserialize_with_mode`] for full description.
///
///  # Errors
///
///  When the parsing or deserilization fails. See [`DeserError`] for details.
pub fn deserialize<P, T>(source: &str) -> Result<T, DeserError<P>>
where
    P: FromTomlTable<T> + VerifyFromToml + std::fmt::Debug,
{
    deserialize_with_mode(source, FieldMatchMode::Exact, VecMode::Strict)
}

/// The main entry point
///
/// Deserialize a TOML string to an `Ok(T)`
/// or, if problems are encountered, an
/// `Err((Vec<[HydratedAnnotatedError]>, PartialT))`
///
///
/// `P` is the `PartialT` written by #[`tpd_make_partial`} on your struct `T`
/// See main documentation page.
///
///  # Errors
///
///  When the parsing or deserilization fails. See [`DeserError`] for details.
///
/// # Panics
///
/// On internal bugs
pub fn deserialize_with_mode<P, T>(
    source: &str,
    match_mode: FieldMatchMode,
    vec_mode: VecMode,
) -> Result<T, DeserError<P>>
where
    P: FromTomlTable<T> + VerifyFromToml + std::fmt::Debug,
{
    let parsed_toml = source
        .parse::<Document<String>>()
        .map_err(|toml_err| DeserError::ParsingFailure(toml_err, source.to_string()))?;
    let source = Rc::new(RefCell::new(source.to_string()));

    let errors = Rc::new(RefCell::new(Vec::new()));
    let col = TomlCollector {
        errors: errors.clone(),
        match_mode,
        vec_mode,
    };
    let mut helper = TomlHelper::from_table(parsed_toml.as_table(), col);

    let partial = P::from_toml_table(&mut helper);

    let partial = partial.verify(&mut helper);
    helper.deny_unknown();

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
        panic!(
            "The Partial was still incomplete, but no error was logged.
Do you have a #[tpd_default_in_verify] field that you're not setting?
Otherwise, this points to a bug in toml_pretty_deser

Partial:
{partial:#?}
"
        );
    }
}

///Nice 'Did you mean one of these: 'A','B' or 'C' suggestions.
pub fn suggest_alternatives<T: AsRef<str>>(current: &str, available: &[T]) -> String {
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
        [single] => format!("'{single}'"),
        [first, second] => format!("'{first}' or '{second}'"),
        rest => {
            let (last, init) = rest.split_last().expect("can't fail");
            let start = init
                .iter()
                .map(|s| format!("'{s}'"))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{start}, or '{last}'")
        }
    }
}

#[doc(hidden)]
#[must_use]
pub fn suggest_enum_alternatives<E: StringNamedEnum>(current: &str) -> String {
    suggest_alternatives(current, E::all_variant_names())
}

/// The failure states of deserialization
///
/// Use `DeserError.pretty(source_name)` to get a nice error message with code snippets and hints.
#[derive(Debug)]
pub enum DeserError<P> {
    /// TOML parsing failed. No `PartialT` available.
    /// A wrapper around [`toml_edit::TomlError`]
    ParsingFailure(TomlError, String),
    /// Parsing suceeded, but deserialization failed. `PartialT` available with whatever could be
    /// understood.
    DeserFailure(Vec<HydratedAnnotatedError>, P),
}

impl<P> DeserError<P> {
    pub fn pretty(&self, toml_filename: &str) -> String {
        let mut out = String::new();
        match self {
            Self::ParsingFailure(toml_error, source) => {
                let spans = vec![SpannedMessage {
                    span: toml_error.span().unwrap_or(0..0),
                    msg: toml_error.message().to_string(),
                }];
                out.push_str(&pretty_error_message(
                    toml_filename,
                    source,
                    &spans,
                    Some(&"See the TOML Spec: https://toml.io/en/v1.1.0".to_string()),
                ));
            }
            Self::DeserFailure(items, _) => {
                let total = items.len();
                for (ii, item) in items.iter().enumerate() {
                    let ii = ii + 1;
                    let _ = write!(out, "Error {ii}/{total}\n");
                    out.push_str(&item.pretty(toml_filename));
                    out.push('\n');
                }
            }
        }
        out
    }
}

/// Deser complex values from TOML tables.
///
/// All known implementations get created by [`toml_pretty_deser_macros::tpd_make_partial`]
///
/// You shouldn't need to implement this, use `FromTomlItem` instead.
///
pub trait FromTomlTable<T> {
    fn can_concrete(&self) -> bool;
    fn from_toml_table(helper: &mut TomlHelper<'_>) -> Self
    where
        Self: Sized;
    fn to_concrete(self) -> Option<T>;
}

/// Your main hook into verifying your values
pub trait VerifyFromToml {
    #[must_use]
    fn verify(self, _helper: &mut TomlHelper<'_>) -> Self
    where
        Self: Sized,
    {
        self
    }
}

/// The non-pretty representation of deser errors.
#[derive(Debug, Clone)]
pub struct AnnotatedError {
    // spans on the TOML source and the messages we should display for them.
    pub spans: Vec<SpannedMessage>,
    /// A helpful hint that's not show inline with the code
    pub help: Option<String>,
}

#[derive(Debug, Clone)]
pub struct SpannedMessage {
    pub span: Range<usize>,
    pub msg: String,
}

/// The user (developer) facing representation of issues with your TOML deserialization
///
/// Combines an [`AnnotatedError`] with the source TOML string to be able to produce pretty error
/// messages with code snippets and hints.
pub struct HydratedAnnotatedError {
    pub source: Rc<RefCell<String>>,
    pub inner: AnnotatedError,
}

impl std::fmt::Debug for HydratedAnnotatedError {
    #[mutants::skip]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "HydratedAnnotatedError {{ ")?;
        write!(f, "{}", self.pretty("debug.toml"))?;
        writeln!(f, " }}")?;
        Ok(())
    }
}

impl AnnotatedError {
    #[must_use]
    pub fn unplaced(help: &str) -> Self {
        Self {
            spans: vec![],
            help: Some(help.to_string()),
        }
    }

    #[must_use]
    pub fn placed(span: Range<usize>, msg: &str, help: &str) -> Self {
        Self {
            spans: vec![SpannedMessage {
                span,
                msg: msg.to_string(),
            }],
            help: Some(help.to_string()),
        }
    }
}

/// Convenience methods for building up `AnnotatedErrors`
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

fn pretty_error_message(
    source_name: &str,
    source: &str,
    spans: &Vec<SpannedMessage>,
    help: Option<&String>,
) -> String {
    use bstr::{BStr, ByteSlice};
    use codesnake::{Block, CodeWidth, Label, LineIndex};
    use std::fmt::Write;

    let source = source;

    if spans.is_empty() {
        format!(
            "ConfigError at unknown location. Help text: {}",
            help.as_ref().map_or("None available", |x| x.as_str())
        )
    } else {
        let idx = LineIndex::new(source);
        let mut spans = spans.clone();
        spans.sort_by_key(|span| span.span.start);

        let mut previous_newline =
            memchr::memmem::rfind(&source.as_bytes()[..spans[0].span.start], b"\n");
        let this_line_is_block_start = source.as_bytes()[previous_newline.unwrap_or(0)..]
            .trim_ascii_start()
            .starts_with(b"[");
        if this_line_is_block_start {
            previous_newline = None;
        }

        let mut labels = Vec::new();

        for span in spans.clone() {
            labels.push(Label::new(span.span).with_text(span.msg));
        }

        let block = Block::new(&idx, labels).unwrap_or_else(|| {
                let mut spans = spans.clone();
                spans.sort_by_key(|span| span.span.start);
                let span_str: Vec<_> = spans
                    .iter()
                    .map(|span| format!("{}..{}: {}", span.span.start, span.span.end, span.msg))
                    .collect();
                let span_str = span_str.join("\n");
                let final_message = format!(
                    "Error spans were overlapping so we were unable to process a pretty code block. Spans & messages:\n{span_str}"
                );
                let labels = vec![Label::new(0..0).with_text(final_message)];
                Block::new(&idx, labels).expect("can not fail")
            });

        let (lines_before, digits_needed) = match previous_newline {
            None => (String::new(), 1),
            Some(previous_newline) => {
                let upto_span = &BStr::new(source.as_bytes())[..previous_newline];
                let lines: Vec<_> = upto_span.lines().collect();
                let str_line_no = format!("{}", lines.len());
                let digits_needed = str_line_no.len();
                let mut seen_opening = false;
                let mut lines_before: Vec<_> = lines
                    .into_iter()
                    .enumerate()
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
        let blockf: String = format!("{block}")
            .lines()
            .skip(1)
            .fold(String::new(), |acc, line| acc + line + "\n");
        write!(&mut out, "{blockf}").expect("can't fail");
        writeln!(&mut out, "{}", block.epilogue()).expect("can't fail");

        if let Some(help) = help.as_ref()
            && !help.is_empty()
        {
            let mut first = true;
            write!(&mut out, "Hint: ").expect("Can't fail");
            for line in help.lines() {
                if !first {
                    write!(&mut out, "      ").expect("can't fail");
                }
                first = false;
                writeln!(&mut out, "{line}").expect("can't fail");
            }
        }
        out
    }
}

impl HydratedAnnotatedError {
    #[must_use]
    pub fn pretty(&self, source_name: &str) -> String {
        pretty_error_message(
            source_name,
            self.source.borrow().as_str(),
            &self.inner.spans,
            self.inner.help.as_ref(),
        )
    }
}

/// Stores information about expected fields and their aliases
#[doc(hidden)]
#[derive(Debug, Clone)]
pub struct FieldInfo {
    pub name: String,
    pub aliases: Vec<&'static str>,
}

impl FieldInfo {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            aliases: Vec::new(),
        }
    }

    #[must_use]
    pub fn with_alias(mut self, alias: &'static str) -> Self {
        self.aliases.push(alias);
        self
    }

    #[must_use]
    pub fn with_aliases(mut self, aliases: &'static [&'static str]) -> Self {
        self.aliases.extend(aliases);
        self
    }

    /// Get all normalized names for this field (primary name + aliases)
    #[must_use]
    pub fn all_normalized_names(&self, mode: &FieldMatchMode) -> Vec<String> {
        let mut names = vec![mode.normalize(&self.name)];
        for alias in &self.aliases {
            names.push(mode.normalize(alias));
        }
        names
    }
}

/// Parameter to [`deserialize_with_mode`]
#[derive(Clone, Debug)]
pub enum VecMode {
    /// Accept single values in lieu of ```[value]```
    SingleOk,
    /// Vecs must be TOML arrays. Always.
    Strict,
}

/// Grab bag that collects the errors and provides parameterisation to
/// the deser functions
#[derive(Clone, Debug)]
pub struct TomlCollector {
    pub errors: Rc<RefCell<Vec<AnnotatedError>>>,
    pub match_mode: FieldMatchMode,
    pub vec_mode: VecMode,
}

/// TOML 'table-like' access wrapper that e.g. verifies that no unexpected fields occur in your TOML
pub struct TomlHelper<'a> {
    table: Option<&'a dyn TableLikePlus>,
    /// Expected field info (what we allow to see)
    expected: Vec<FieldInfo>,
    /// Normalized names that were actually observed (matched against table keys)
    observed: Vec<String>,
    /// Original field names that were allowed to the keys observed
    pub col: TomlCollector,
}

impl<'a> TomlHelper<'a> {
    pub fn from_table(table: &'a dyn TableLikePlus, col: TomlCollector) -> Self {
        Self {
            table: Some(table),
            expected: vec![],
            observed: vec![],
            col,
        }
    }

    /// Create a `TomlHelper` from a `toml_edit::Item` (either Table or `InlineTable`)
    #[must_use]
    pub fn from_item(item: &'a toml_edit::Item, col: &TomlCollector) -> Self {
        match item.as_table_like_plus() {
            Some(table) => Self::from_table(table, col.clone()),
            _ => Self {
                table: None,
                expected: vec![],
                observed: vec![],
                col: col.clone(),
            },
        }
    }

    pub fn into_inner(self, source: &Rc<RefCell<String>>) -> Vec<HydratedAnnotatedError> {
        self.col
            .errors
            .borrow_mut()
            .drain(..)
            .map(|error| HydratedAnnotatedError {
                source: source.clone(),
                inner: error,
            })
            .collect()
    }

    /// Register a field with optional aliases
    pub fn expect_field(&mut self, name: impl Into<String>, aliases: &'static [&'static str]) {
        let field_info = FieldInfo::new(name).with_aliases(aliases);
        self.expected.push(field_info);
    }
    /// Register a field with optional aliases
    pub fn ignore_field(&mut self, name: impl Into<String>) {
        let name: String = name.into();
        let field_info = FieldInfo::new(name.clone());
        self.expected.push(field_info);
        self.observed.push(name);
    }

    /// Find a key in the table that matches the given field name (considering aliases and match mode)
    fn find_matching_keys(
        &self,
        name: &str,
        aliases: &[&'static str],
    ) -> Vec<(String, toml_edit::Item)> {
        let mut result = Vec::new();
        let _normalized_target = self.col.match_mode.normalize(name);
        let candidates = std::iter::once(name.to_string())
            .chain(aliases.iter().map(std::string::ToString::to_string))
            .collect::<Vec<_>>();

        // Collect all table keys
        let table_keys: Vec<String> = if let Some(table) = self.table {
            table.iter().map(|(k, _)| k.to_string()).collect()
        } else {
            vec![]
        };

        // Try to find a match
        for table_key in &table_keys {
            for candidate in &candidates {
                if self.col.match_mode.matches(candidate, table_key)
                    && let Some(table) = self.table
                    && let Some(item) = table.get(table_key)
                {
                    result.push((table_key.clone(), item.clone()));
                    break;
                }
            }
        }

        result
    }

    /// # Panics
    /// Shouldn't.
    pub fn get_with_aliases<T>(
        &mut self,
        query_key: &str,
        aliases: &'static [&'static str],
        missing_is_error: bool,
    ) -> TomlValue<T>
    where
        T: FromTomlItem + std::fmt::Debug,
    {
        let parent_span = if let Some(table) = self.table {
            table.span().unwrap_or(0..0)
        } else {
            0..0
        };

        // Register this field as expected
        self.expect_field(query_key, aliases);

        // Try to find a matching key (considering aliases and match mode)
        let found_keys = self.find_matching_keys(query_key, aliases);

        match found_keys.len() {
            0 => {
                // No match found
                let res = TomlValue {
                    value: None,
                    state: TomlValueState::Missing {
                        key: query_key.to_string(),
                        parent_span,
                    },
                };
                if missing_is_error {
                    res.register_error(&self.col.errors);
                }
                res
            }
            1 => {
                let (matched_key, item) = found_keys.first().expect("can't fail");
                let res: TomlValue<T> = FromTomlItem::from_toml_item(item, parent_span, &self.col);
                if !matches!(res.state, TomlValueState::Ok { .. }) {
                    res.register_error(&self.col.errors);
                }
                self.observed
                    .push(self.col.match_mode.normalize(matched_key));
                res
            }
            _ => {
                let spans = found_keys
                    .iter()
                    .map(|(matched_key, _item)| self.span_from_key(matched_key))
                    .collect();
                for (matched_key, _) in &found_keys {
                    self.observed
                        .push(self.col.match_mode.normalize(matched_key));
                }

                let res = TomlValue {
                    value: None,
                    state: TomlValueState::MultiDefined {
                        key: query_key.to_string(),
                        spans,
                    },
                };
                res.register_error(&self.col.errors);
                res
            }
        }
    }

    fn span_from_key(&self, key: &str) -> Range<usize> {
        if let Some(table) = self.table {
            table
                .key(key)
                .and_then(toml_edit::Key::span)
                .unwrap_or(0..0)
        // } else if let Some(inline_table) = self.inline_table {
        //     // For inline tables, we can't easily get the key span, so use the table span
        //     inline_table.span().unwrap_or(0..0)
        } else {
            0..0
        }
    }

    pub fn add_err_by_key(&self, key: &str, msg: &str, help: &str) {
        let span = self.span_from_key(key);
        self.add_err_by_span(span, msg, help);
    }

    pub fn add_err_by_span(&self, span: Range<usize>, msg: &str, help: &str) {
        let err = AnnotatedError::placed(span, msg, help);
        self.col.errors.borrow_mut().push(err);
    }

    #[allow(clippy::missing_panics_doc)]
    pub fn add_err_by_spans(&self, spans: Vec<(Range<usize>, String)>, help: &str) {
        let err = if spans.is_empty() {
            AnnotatedError::unplaced(help)
        } else {
            let mut iter = spans.into_iter();
            let first = iter.next().unwrap();
            let mut err = AnnotatedError::placed(first.0, &first.1, help);
            for (span, msg) in iter {
                err.add_span(span, &msg);
            }
            err
        };
        self.col.errors.borrow_mut().push(err);
    }

    pub fn deny_unknown(&self) {
        // Build set of normalized expected names (including aliases)
        let mut expected_normalized: IndexSet<String> = IndexSet::new();
        for field_info in &self.expected {
            for normalized_name in field_info.all_normalized_names(&self.col.match_mode) {
                expected_normalized.insert(normalized_name);
            }
        }

        // Collect all keys from either table type
        let keys: Vec<String> = if let Some(table) = self.table {
            table.iter().map(|(k, _)| k.to_string()).collect()
        } else {
            vec![]
        };

        // Build set of observed normalized names
        let observed_set: IndexSet<String> = self.observed.iter().cloned().collect();

        for key in keys {
            let normalized_key = self.col.match_mode.normalize(&key);

            // Check if this key was observed (i.e., it matched an expected field)
            if !observed_set.contains(&normalized_key) {
                // This is an unknown key - find available (expected but not yet observed) fields
                let still_available: Vec<_> = expected_normalized
                    .iter()
                    .filter(|expected| !observed_set.contains(*expected))
                    .map(std::string::String::as_str)
                    .collect();

                self.add_err_by_key(
                    &key,
                    "Unknown key.",
                    &suggest_alternatives(&key, &still_available),
                );
            }
        }
    }
}

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
impl_from_toml_item_integer!(i16, "i16");
impl_from_toml_item_integer!(i32, "i32");
impl_from_toml_item_integer!(u32, "u32");
impl_from_toml_item_integer!(i64, "i64");
//impl_from_toml_item_integer!(u64, "u64");

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
impl_from_toml_item_value!(f64, "Float", Float);

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

// Blanket implementation for Vec<T> where T: FromTomlItem
// This handles both regular arrays and ArrayOfTables (for Vec<toml_edit::Item> specifically)
#[allow(clippy::too_many_lines)]
impl<T: FromTomlItem> FromTomlItem for Vec<T> {
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

                    if let TomlValueState::Ok { .. } = &element.state {
                        if let Some(val) = element.value {
                            values.push(val);
                        }
                    } else {
                        element.register_error(&col.errors);
                        has_error = true;
                    }
                }

                if has_error {
                    TomlValue {
                        value: None,
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

                    if let TomlValueState::Ok { .. } = &element.state {
                        if let Some(val) = element.value {
                            values.push(val);
                        }
                    } else {
                        element.register_error(&col.errors);
                        has_error = true;
                    }
                }

                if has_error {
                    TomlValue {
                        value: None,
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
                    if let TomlValueState::Ok { span } = &element.state {
                        TomlValue::new_ok(vec![element.value.expect("unreachable")], span.clone())
                    } else {
                        element.register_error(&col.errors);
                        TomlValue {
                            state: TomlValueState::Nested {},
                            value: None,
                        }
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
                    if let TomlValueState::Ok { span } = &element.state {
                        TomlValue::new_ok(vec![element.value.expect("unreachable")], span.clone())
                    } else {
                        element.register_error(&col.errors);
                        TomlValue {
                            state: TomlValueState::Nested {},
                            value: None,
                        }
                    }
                }
                VecMode::Strict => TomlValue {
                    value: None,
                    state: TomlValueState::WrongType {
                        span: value.span().unwrap_or(parent_span),
                        expected: "array (maybe of tables)",
                        found: "table",
                    },
                },
            },
        }
    }
}

/// The internal representation of a value to-have-been-deserialized
#[derive(Debug)]
pub struct TomlValue<T> {
    pub value: Option<T>,
    pub state: TomlValueState,
}

impl<T> TomlValue<T> {
    pub const fn new_ok(value: T, span: Range<usize>) -> Self {
        Self {
            value: Some(value),
            state: TomlValueState::Ok { span },
        }
    }

    #[must_use]
    pub const fn new_empty_missing(parent_span: Range<usize>) -> Self {
        Self {
            value: None,
            state: TomlValueState::Missing {
                key: String::new(),
                parent_span,
            },
        }
    }
    #[must_use]
    pub const fn new_validation_failed(
        span: Range<usize>,
        message: String,
        help: Option<String>,
    ) -> Self {
        Self {
            value: None,
            state: TomlValueState::ValidationFailed {
                span,
                message,
                help,
            },
        }
    }

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
        }
    }

    pub const fn has_value(&self) -> bool {
        matches!(self.state, TomlValueState::Ok { .. })
    }

    pub fn into_option(self) -> Option<T> {
        match self.state {
            TomlValueState::Ok { .. } => self.value,
            _ => None,
        }
    }

    pub fn into_optional(self) -> TomlValue<Option<T>> {
        match self.state {
            TomlValueState::Ok { span } => TomlValue {
                value: Some(self.value),
                state: TomlValueState::Ok { span },
            },
            TomlValueState::Missing {
                key: _,
                parent_span,
            } => TomlValue {
                value: Some(None),
                state: TomlValueState::Ok { span: parent_span },
            },
            _ => TomlValue {
                value: None,
                state: self.state,
            },
        }
    }

    pub const fn as_ref(&self) -> Option<&T> {
        match self.state {
            TomlValueState::Ok { .. } => self.value.as_ref(),
            _ => None,
        }
    }

    pub fn span(&self) -> Range<usize> {
        match &self.state {
            TomlValueState::Ok { span } |
            TomlValueState::Missing { parent_span: span, .. } |
            TomlValueState::WrongType { span, .. } |
            TomlValueState::ValidationFailed { span, .. } => span.clone(),
            TomlValueState::NotSet | TomlValueState::Nested => 0..0,
            TomlValueState::MultiDefined { spans, .. } => spans[0].clone(), //just return the first one
        }
    }

    /// # Panics
    /// When the value is not `TomlValueState::Ok`
    pub fn expect(self, msg: &str) -> T {
        match self.state {
            TomlValueState::Ok { .. } => self.value.expect(msg),
            _ => panic!("Called unwrap on a TomlValue that is not Ok"),
        }
    }

    pub fn register_error(&self, errors: &Rc<RefCell<Vec<AnnotatedError>>>) {
        match &self.state {
            TomlValueState::NotSet |
            TomlValueState::Ok { .. } |
            TomlValueState::Nested => {} //ignored, we expect the errors below to have been added
            TomlValueState::Missing { key, parent_span } => {
                errors.borrow_mut().push(AnnotatedError::placed(
                    parent_span.clone(),
                    &format!("Missing required key: '{key}'."),
                    "This key is required but was not found in the TOML document.",
                ));
            }
            TomlValueState::MultiDefined { key, spans } => {
                let mut err = AnnotatedError::placed(
                    spans[0].clone(),
                    "Key/alias conflict (defined multiple times)",
                    &format!("Use only one of the keys involved. Canonical is '{key}'"),
                );
                for span in spans.iter().skip(1) {
                    err.add_span(span.clone(), "Also defined here");
                }
                errors.borrow_mut().push(err);
            }
            TomlValueState::WrongType {
                span,
                expected,
                found,
            } => {
                errors.borrow_mut().push(AnnotatedError::placed(
                    span.clone(),
                    &format!("Wrong type: expected {expected}, found {found}"),
                    "This value has the wrong type.",
                ));
            }
            TomlValueState::ValidationFailed {
                span,
                message,
                help,
            } => {
                errors.borrow_mut().push(AnnotatedError::placed(
                    span.clone(),
                    message,
                    help.as_ref().map_or("", std::string::String::as_str),
                ));
            }
        }
    }

    #[allow(clippy::missing_panics_doc)]
    #[must_use]
    pub fn verify<F>(self, helper: &mut TomlHelper, verification_func: F) -> Self
    where
        F: FnOnce(&T) -> Result<(), (String, Option<String>)>,
    {
        match &self.state {
            TomlValueState::Ok { span } => match verification_func(
                self.value
                    .as_ref()
                    .expect("None value on TomlValueState::Ok"),
            ) {
                Ok(()) => self,
                Err((msg, help)) => {
                    let res = Self {
                        value: None,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: msg,
                            help, //todo
                        },
                    };
                    res.register_error(&helper.col.errors);
                    res
                }
            },
            _ => self,
        }
    }

    #[must_use]
    pub fn or_default(self, default: T) -> Self {
        match &self.state {
            TomlValueState::Missing { .. } => Self {
                value: Some(default),
                state: TomlValueState::Ok { span: 0..0 },
            },
            _ => self,
        }
    }
}

#[doc(hidden)]
#[must_use]
pub fn deserialize_nested<P, T>(
    item: &toml_edit::Item,
    span: &Range<usize>,
    col: &TomlCollector,
    fields_to_ignore: &[&str],
) -> TomlValue<P>
where
    P: FromTomlTable<T> + VerifyFromToml,
{
    match item.as_table_like_plus() {
        Some(table) => {
            let mut helper = TomlHelper::from_table(table, col.clone());
            for f in fields_to_ignore {
                helper.ignore_field(*f);
            }
            let partial = P::from_toml_table(&mut helper).verify(&mut helper);
            helper.deny_unknown();

            if partial.can_concrete() {
                TomlValue {
                    value: Some(partial),
                    state: TomlValueState::Ok { span: span.clone() },
                }
            } else {
                TomlValue {
                    value: Some(partial),
                    state: TomlValueState::ValidationFailed {
                        span: span.clone(),
                        message: "Nested struct has errors".to_string(),
                        help: None,
                    },
                }
            }
        }
        None => TomlValue {
            value: None,
            state: TomlValueState::WrongType {
                span: span.clone(),
                expected: "table or inline table",
                found: "other type",
            },
        },
    }
}

// ================================
// IndexMap / Map Support
// ================================

///Internally called by the macros
#[doc(hidden)]
#[must_use]
pub fn toml_item_as_map<T: FromTomlItem>(
    toml_item: &TomlValue<toml_edit::Item>,
    col: &TomlCollector,
) -> TomlValue<IndexMap<String, T>> {
    match &toml_item.state {
        TomlValueState::Ok { span } => {
            if let Some(ref item) = toml_item.value {
                match item.as_table_like_plus() {
                    Some(table) => {
                        let mut map = IndexMap::new();
                        let mut has_errors = false;

                        for (key, value) in table.iter() {
                            let item_span = value.span().unwrap_or(span.clone());
                            let val: TomlValue<T> =
                                FromTomlItem::from_toml_item(value, item_span.clone(), col);
                            if let TomlValueState::Ok { .. } = val.state {
                                if let Some(v) = val.value {
                                    map.insert(key.to_string(), v);
                                }
                            } else {
                                val.register_error(&col.errors);
                                has_errors = true;
                            }
                        }

                        if has_errors {
                            TomlValue {
                                value: Some(map),
                                state: TomlValueState::Nested {},
                            }
                        } else {
                            TomlValue {
                                value: Some(map),
                                state: TomlValueState::Ok { span: span.clone() },
                            }
                        }
                    }
                    None => TomlValue {
                        value: None,
                        state: TomlValueState::WrongType {
                            span: span.clone(),
                            expected: "table|inline_table",
                            found: item.type_name(),
                        },
                    },
                }
            } else {
                TomlValue {
                    value: None,
                    state: TomlValueState::ValidationFailed {
                        span: span.clone(),
                        message: "Cannot convert empty value to map".to_string(),
                        help: None,
                    },
                }
            }
        }
        _ => TomlValue {
            value: None,
            state: toml_item.state.clone(),
        },
    }
}

/// Captures precisely what went wrong
#[derive(Debug, Clone)]
pub enum TomlValueState {
    NotSet,
    Missing {
        key: String,
        parent_span: Range<usize>,
    },
    MultiDefined {
        key: String,
        spans: Vec<Range<usize>>,
    },
    WrongType {
        span: Range<usize>,
        expected: &'static str,
        found: &'static str,
    },
    ValidationFailed {
        span: Range<usize>,
        message: String,
        help: Option<String>,
    },
    Nested,
    Ok {
        span: Range<usize>,
    },
}
