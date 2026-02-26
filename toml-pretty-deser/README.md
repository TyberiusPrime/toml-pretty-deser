# toml-pretty-deser

Deserialize [TOML 1.1.0](https://toml.io/en/v1.1.0) with pretty error messages.

[![docs.rs](https://img.shields.io/docsrs/toml-pretty-deser?version=0.3.0)](https://docs.rs/toml-pretty-deser)

## Examples

See the [examples folder](https://github.com/TyberiusPrime/toml-pretty-deser/tree/main/toml-pretty-deser/examples) for self-contained, runnable examples covering all features.

## Usage:

Deserialize your configuration structs from TOML with beautiful error message,
and keeping whatever could be understood around in case of errors:

```toml
        a = 5
        b = 10
        c = 3
        d = 100
```

becomes

```text
Error 1/2
  ╭─example.toml
  ┆
2 │         a = 5
  ┆             ┬
  ┆             │
  ┆             ╰─ a+b+c must add up to 99. Sum was 18.
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
Hint: For demonstration purposes only
```

Using this client Rust code:

```rust
use toml_pretty_deser::prelude::*;

#[tpd(root)]
struct ShowOffTwoValueErrors {
    a: i64,
    b: i64,
    c: i64,
    d: i64,
}

impl VerifyIn<TPDRoot> for PartialShowOffTwoValueErrors {
    fn verify(
        &mut self,
        parent: &TPDRoot,
    ) -> Result<(), ValidationFailure>
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
                        format!("a+b+c must add up to 99. Sum was {sum}."),
                    ),
                    (self.b.span(), "See a".to_string()),
                    (self.c.span(), "See a".to_string()),
                ];
                self.a = TomlValue::new_custom(
                    self.a.value,
                    spans, Some("For example, set a = 33, b=66, c=0"));
            }
        }

        //in place verification, no assignment required
        self.d.verify(|value| { //the take is necessary
            if *value < 50 {
                Ok(())
            } else {
               Err(ValidationFailure::new(
                   "Must be below 50",
                   Some("For demonstration purposes only")
               ))
            }
        });

        //returning err here adds another top level error here
        Ok(())

    }
}
let toml = "
        a = 5
        b = 10
        c = 3
        d = 100
        ";

    let result = ShowOffTwoValueErrors::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    if let Err(err) = result {
        println!("{}", err.pretty("example.toml"));
    }
```

## Supported types

- 'simple' types: integers, strings, booleans, `std::path::PathBufs`, toml_edit::Items
- Nested structs (tag struct definition with `#[tpd]` and use with `#[tpd(nested)]`
- String->simple Enums  (tag enum definition with `#[tpd]`
- Table->tagged enums with one struct each (`#[tpd(tag="toml-key")]` on enum, `#[tpd]` on inner structs, `#[tpd(nested)]` on fields
- Vectors of the above
- `IndexMaps<FromString, One-of-the-above>`

toml-pretty-deser explicitly does not support other map types (like `HashMap`).
That is because we want to keep the configuration's order.

## How this works:

`#[tpd]` writes a `PartialT` for every struct T you apply it on,
and implementations to go from `toml_edit` types to your `PartialT`, as well
as a conversion to turn complete `PartialT` back into T, all using the visitor pattern.

The `PartialT` consists of all the same fields as your `T`, wrapped in `TomlValue` - which records the
deserialization state and where in the TOML document it was.

`T::tpd_from_toml` will then give you an Ok(T) or an Err with a list of errors and the partial
state (If parsing succeeded. Otherwise you get just a `DeserError::ParsingFailure`. See [`DeserError`].


The errors contain information about what went wrong, and can turn themselves into pretty error messages
as in the example above.

## Why not just serde

Serde is great. If it works for you, good, use it!.
It's got way more battle hardening than this.

This offers:
- partial deserialization
- pretty, highly targeted error messages
- multiple errors at once
- the ability to access the partial parent during verification
- optionally snake-case (anyCase, actually) and upper/lower case insensitivity.



## Usage Variations

### 

### No custom validation

Use `#[tpd(no_verify)]` on your struct, and skip writing a [`VerifyIn`] implementation.

### Skipping fields

At times, you want fields that are not present in your TOML
on your structs. For example, file handles derived from the filenames supplied
by your user.

You can do this by setting the field to `#[tdp(skip)]`.

These fields get placed as Option<F> instead of TomlValue<F>
inside the partial. You can set them in your VerifyIn implementation
if required. The get turned into concrete values by .unwrap_or_default()
on the Option.


### Defaulting fields

Fields tagged with `#[tpd(default)]` get set to their default::Default() value
if they are missing (not if they fail otherwise!).

Alternatively, you can always apply defaults in your `VerifyIn` implementation
```rust, ignore
impl VerifyIn<TPDRoot> for PartialWithDefaults {
    fn verify(
        &mut self,
        _parent: &TPDRoot,
    ) -> Result<(), (String, Option<String>)>
    self.field = self.field.take().or_default() // equivalent to #[tpd(default)]
    self.field = self.field.take().or(435) // const value
    self.field = self.field.take().or_with(|| 435) // closure evaluated
```




### Nested structs

When you want to represent `NestedT` as `PartialNestedT` inside your struct T,
tag them with `#[tpd(nested)]`


### Enums

For simple string-typed Enums without an inner payload, tag the enum declaration with
`#[tpd]`.

For deserializing tagged Enums with a struct payload,
use `#[tpd(tag="toml_key")]` on the enum declaration,
and pass in the tag key name (and it's aliases).

Example:

```rust
use toml_pretty_deser::prelude::*;
#[tpd(no_verify)]
#[derive(Debug)]
struct InnerA {
    n: i32,
    o: u32,
}

#[tpd(no_verify)]
#[derive(Debug)]
struct InnerB {
    s: u32,
    t: u32,
}

#[tpd(tag="kind", alias="type")]
#[derive(Debug)]
enum EitherOne {
    KindA(InnerA),
    KindB(Box<InnerB>), // using a Box is supported.
}
```


`PatrialEither` exposes a method `get_tpd_tag()` to receive the canonical 
string name.


### Aliases

You can supply alias names on any field using `#[tpd(alias="another-name", alias="yet-another-name")]`.
This can be repeated.

To allow aliases on a tagged enum's tag field, 
use `#[tdp(tag="kind", alias="type", alias="tag")]`

Note the section below on casing for the common case of case- or code-case-insensitivity


### Casing

By default, field names and enum variants are matched strictly.

`tpd_from_toml`'s `FieldMatchMode` argument allows you to change this to case-insensitive (upper/lower only) or
code-case-insensitive (allowing '`snake_case`', 'camelCase', 'kebab-case', etc. to match each other).

### Accessing the field match mode in verify

Every generated `PartialT` struct includes a `pub tpd_field_match_mode: FieldMatchMode` field
that is automatically set to the match mode used during deserialization.
`TPDRoot` (the parent type for top-level structs) likewise exposes `field_match_mode`.

You can read either of these in your [`VerifyIn`] implementation to make mode-aware validation decisions:

```rust, ignore
// For a nested struct: read from self
impl VerifyIn<PartialOuter> for PartialMyStruct {
    fn verify(&mut self, _parent: &PartialOuter) -> Result<(), ValidationFailure>
    where Self: Sized + Visitor
    {
        if self.tpd_field_match_mode == FieldMatchMode::Exact {
            // stricter checks when exact matching is required
        }
        Ok(())
    }
}

// For a root struct: read from parent (TPDRoot)
impl VerifyIn<TPDRoot> for PartialMyRoot {
    fn verify(&mut self, parent: &TPDRoot) -> Result<(), ValidationFailure>
    where Self: Sized + Visitor
    {
        if parent.tpd_field_match_mode == FieldMatchMode::AnyCase {
            // relaxed checks when case-insensitive matching is in use
        }
        Ok(())
    }
}
```


### Single elements to Vecs

By default, when a field is `Vec<T>`, a TOML list must be provided.
`tpd_from_toml` `VecMode` argument allows you to change this to allow single elements to be treated as
one-element vectors instead.


### Adapter functions


Use an attribute `#[tpd(with="function_name)"]` on the field:

Example:

```rust
use toml_pretty_deser::prelude::*;

#[tpd(root, no_verify)]
struct ExampleAdapt {
    #[tpd(with="adapt_to_upper_case")]
    name: String,

}

pub fn adapt_to_upper_case(input: TomlValue<String>) -> TomlValue<String> {
    input.map(|s| s.to_uppercase())
}
```

### Custom types

To enable the deserialization of custom types, implement [`Visitor`] for them.

If they have no inner fields to descend into, 
you can use the [`impl_visitor`] macro (see below).
If they do, plesae read the other Visitor implementation.


```rust
use toml_pretty_deser::prelude::*;
struct DNA(String);

impl_visitor!(DNA, |helper| {
    match helper.item.as_str() {
        Some(s) => {
                if s.chars()
                    .all(|c| matches!(c, 'a' | 'c' | 'g' | 't' | 'A' | 'C' | 'G' | 'T'))
                {
                    TomlValue::new_ok(DNA(s.to_string()), helper.span())
                } else {
                    TomlValue::new_validation_failed(
                        helper.span(),
                        "Invalid base".to_string(),
                        Some("Use only AGTC".to_string()),
                    )
                }
            }
        None => TomlValue::new_wrong_type(&helper.item, helper.span(), "String"),
    }
});

```

### Adapt in VerifyIn

On occasion, you need the parent to really to perform final validation, 
for example if you wanted to store a lookup into a vec on the parent.

Or you need to adapt into a Wrapped type, like Rc::RefCell

For this, you may tag fields with `#[tpd(adapt_in_verify(type))]`.
These field get first deserialized into a `MustAdapt::PreVerify(type)`,
and you must turn them into `MustAdapt::PostVerify(field_type)` in your VerifyIn
implementation (or you will receive a `DeserError`).

There is a convenient wrapper fn adapt(|pre_value, span|) for this which only 
requires you to return a TomlValue(post_value), not a MustAdapt.

Combining adapt_in_verify and nested is special cased. You do not need (or should)
specify the pre-verification type, it autodetects the inner most type.
Since `adapt()` will only call it's callback if the partialT was ok,
it will be called with a T already, and you only need turn it into 
your final output type.




Example:

```rust
use toml_pretty_deser::prelude::*;
use std::{rc::Rc, cell::RefCell};

#[derive(Debug)]
#[tpd(root)]
pub struct AdaptInVerify {
    #[tpd(adapt_in_verify)] //default to toml_edit::Item if not type passed.
    inner: usize,
    #[tpd(adapt_in_verify(String))]
    other: usize,

    #[tpd(adapt_in_verify, nested)]
    nested: Rc<RefCell<Nested>>,
}
#[derive(Debug)]
#[tpd(no_verify)]
struct Nested {
    level1: u8
}

impl VerifyIn<TPDRoot> for PartialAdaptInVerify {
    fn verify(&mut self, _parent: &TPDRoot) -> Result<(), ValidationFailure>
    where
        Self: Sized + toml_pretty_deser::Visitor,
    {
        self.other.adapt(|value| (value.len(), TomlValueState::Ok));

        self.inner.adapt(|value| {
            let found = value.type_name();
            match value.as_str() {
                Some(v) => (v.len(), TomlValueState::Ok),
                None => (0, TomlValueState::WrongType { expected: "string", found }),
            }
        });

        self.nested.adapt(|value| (Rc::new(RefCell::new(value)), TomlValueState::Ok));
        Ok(())
    }
}
```


### Verifying Map keys

To enable the verification of map keys, `IndexMaps` get stored into 
`MapAndKeys` inside `PartialT`s.

This allows you to verify & fail them in VerifyIn:
```rust
use toml_pretty_deser::prelude::*;
use indexmap::IndexMap;

#[derive(Debug)]
#[tpd(root)]
#[allow(dead_code)]
pub struct MapKeyNotStartsWithA {
    a_map: IndexMap<String, u8>,
}

impl VerifyIn<TPDRoot> for PartialMapKeyNotStartsWithA {
    fn verify(&mut self, _parent: &TPDRoot) -> Result<(), ValidationFailure>
    where
        Self: Sized + toml_pretty_deser::Visitor,
    {
        self.a_map.verify_keys(|key_string| {
            if key_string.starts_with("A") || key_string.starts_with("a") {
                Err(ValidationFailure::new(
                    "Keys cannot start with 'A'",
                    Some("Help text goes here"),
                ))
            } else {
                Ok(())
            }
        });
        Ok(())
    }
}
```




### Absorb remaining

By default, non-matching keys are errors.

You can absorb all non-matching keys into a field using 
`#[tpd(absorb_remaining)]`

Example:


```rust

use toml_pretty_deser::prelude::*;
use indexmap::IndexMap;

#[tpd(root, no_verify)]
struct ExampleAbsorb {
    #[tpd(nested)]
    options: Options,
    #[tpd(absorb_remaining)]
    input_files: IndexMap<String, String>,
}

#[derive(Debug)]
#[tpd(no_verify)]
struct Options {
    decompress: bool
}

```
will read TOMLs like
```TOML
fileA = 'hello.txt'
fileB = 'world.txt'
options.decompress = true
```


### "the trait bound `InnerStruct:Visitor` is not satisfied on T"

You're likely missing the `#[tpd(nested)]` on your field definition,
the Visitor trait is implemented by `#[tpd]` for the `PartialT` 
not the `T`



## Changelog

See [CHANGELOG.md]
