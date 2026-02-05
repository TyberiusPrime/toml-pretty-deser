# toml-pretty-deser

Deserialize [TOML 1.1.0](https://toml.io/en/v1.1.0) with pretty error messages.

//! [![docs.rs](https://img.shields.io/docsrs/toml-pretty-deser?version=0.1.0)](https://docs.rs/toml-pretty-deser)

### Usage:

Deserialize your configuration structs from TOML with beautiful error message,
and keeping whatever could be understood around in case of errors:

```toml
        a = 5
        b = 10
        c = 3
```

becomes

```
Error 1/2
  ╭─example.toml
  ┆

2 │         a = 5
  ┆             ┬
  ┆             │
  ┆             ╰─ a+b+c must add up to 100. Sum was 18.
3 │         b = 10
  ┆             ─┬
  ┆              │
  ┆              ╰─ See a
4 │         c = 3
  ┆             ┬
  ┆             │
  ┆             ╰─ See a
──╯
Hint: For example, set a = 33, b=66, c=0

Error 2/2
  ╭─example.toml
  ┆
1 │
2 │         a = 5
3 │         b = 10
4 │         c = 3
5 │         d = 100
  ┆             ─┬─
  ┆              │
  ┆              ╰── Must be below 50
──╯
Hint: For demonstration purposes only`

Using this client Rust code:

```rust
use toml_pretty_deser::prelude::*;
#[tpd_make_partial(false)]
struct ShowOffTwoValueErrors {
    a: i64,
    b: i64,
    c: i64,
    d: i64,
}

impl VerifyFromToml for PartialShowOffTwoValueErrors {
    fn verify(mut self, helper: &mut TomlHelper<'_>) -> Self
    where
        Self: Sized,
    {
        if let Some(a) = self.a.value
            && let Some(b) = self.b.value
            && let Some(c) = self.c.value
        {
            let sum = a + b + c;
            if sum != 99 {
                let spans = vec![
                    (
                        self.a.span(),
                        format!("a+b+c must add up to 100. Sum was {sum}."),
                    ),
                    (self.b.span(), "See a".to_string()),
                    (self.c.span(), "See a".to_string()),
                ];
                helper.add_err_by_spans(spans, "For example, set a = 33, b=66, c=0")
            }
        }

        self.d = self.d.verify(helper, |value| {
            if *value < 50 {
                Ok(())
            } else {
               Err((
                   "Must be below 50".to_string(),
                   Some("For demonstration purposes only".to_string())
               ))
            }
        });

        self
    }
}
let toml = "
        a = 5
        b = 10
        c = 3
        d = 100
        ";

    let result = deserialize::<PartialShowOffTwoValueErrors, ShowOffTwoValueErrors>(toml);
    if let Err(err) = result {
        println!("{}", err.pretty("example.toml"));
    }
```

### How this works:

`#[tpd_make_partial]` writes a `PartialT` for every struct T you apply it on,
and implementations to go from `toml_edit` types to your `PartialT`, as well
as a conversion to turn complete `PartialT` back into T.

The `PartialT` consists of all the same fields, wrapped in `TomlValue` - which records the
deserialization state and where in the TOML document it was.

`deserialize` will then give you an Ok(T) or an Err with a list of errors and the partial
state (If parsing succeeded. Otherwise you get just a parse error). See [`DeserError`]


The hydrated errors contain information about what went wrong, and can turn themselves into pretty error messages
as in the example above.

### Why not just serde

Serde is great. If it works for you, good, use it!.

But I want pretty error messages, more than one error reported at once,
pin-point accuracy in the errors and the ability to continue on with incomplete
configurations. Oh, and optional case and snake-case insensitivity.


### Usage Variations

#### No custom validation

Use `tpd_make_partial(true)` or plain `tpd_make_partial`.

`VerifyFromToml for PartialT` will be produced by the macro.

#### Nested structs

When you want to represent `NestedT` as `PartialNestedT` inside your struct T,
tag them with `#[tpd_nested]`

Complete error checking is provided either way.

#### Enums


For simple string-typed Enums without an inner payload, tag the enum declaration with
[`toml_pretty_deser_macros::tpd_make_enum`].

For deserializing tagged Enums with a struct payload,
use [`toml_pretty_deser_macros::tpd_make_tagged_enum`] on the enum declaration,
and pass in the tag key name (and it's aliases).

Example:

```rust
use toml_pretty_deser::prelude::*;
#[tpd_make_partial]
#[derive(Debug)]
struct InnerA {
    n: i32,
    o: u32,
}

#[tpd_make_partial]
#[derive(Debug)]
struct InnerB {
    s: u32,
    t: u32,
}

#[tpd_make_tagged_enum("kind", aliases = ["type"])]
#[derive(Debug)]
enum EitherOne {
    KindA(InnerA),
    KindB(InnerB),
}
```


### Aliases

You can supply alias names on any field using `[#tpd_alias(name1, name2]`.
Note the section below on casing for the common case of case- or code-case-insensitivity


### Casing

By default, field names and enum variants are matched strictl.

[`deserialize_with_mode`] allows you to change this to case-insensitive (upper/lower only) or
code-case-insensitive (allowing `snake_case`, camelCase, kebab-case, etc. to match each other).


### Single elements to Vecs

By default, when a field is `Vec<T>`, a TOML list must be provided.
[`deserialize_with_mode`] allows you to change this to allow single elements to be treated as
one-element Vecs instead.


### Custom types

To enable the deserialization of custom types, implement [`FromTomlItem`] for them.
```rust
use toml_pretty_deser::prelude::*;
struct DNA(String);
impl FromTomlItem for DNA {
    fn from_toml_item(
        item: &toml_edit::Item,
        parent_span: std::ops::Range<usize>,
        _col: &TomlCollector,
    ) -> TomlValue<DNA> {
        match item.as_str() {
            Some(s) => {
                if s.chars()
                    .all(|c| matches!(c, 'a' | 'c' | 'g' | 't' | 'A' | 'C' | 'G' | 'T'))
                {
                    TomlValue::new_ok(DNA(s.to_string()), parent_span)
                } else {
                    TomlValue::new_validation_failed(
                        item.span().unwrap_or(parent_span),
                        "Invalid base".to_string(),
                        Some("Use only AGTC".to_string()),
                    )
                }
            }
            None => TomlValue::new_wrong_type(item, parent_span, "String(DNA)"),
        }
    }
}
```

