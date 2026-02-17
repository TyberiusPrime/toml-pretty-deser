# toml-pretty-deser

Deserialize [TOML 1.1.0](https://toml.io/en/v1.1.0) with pretty error messages.

[![docs.rs](https://img.shields.io/docsrs/toml-pretty-deser?version=0.3.0)](https://docs.rs/toml-pretty-deser)

### Usage:

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

impl VerifyIn<Root> for PartialShowOffTwoValueErrors {
    fn verify(
        &mut self,
        helper: &mut TomlHelper<'_>,
        parent: &Root,
    ) -> Result<(), (String, Option<String>)>
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

        self.d = self.d.take().verify(|value| { //the take is necessary
            if *value < 50 {
                Ok(())
            } else {
               Err((
                   "Must be below 50".to_string(),
                   Some("For demonstration purposes only".to_string())
               ))
            }
        });

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

### Supported types

- 'simple' types: integers, strings, booleans, `std::path::PathBufs`
- Nested structs (tag struct definition with `#[tpd]` and use with `#[tpd(nested)]`
- String->simple Enums  (tag enum definition with `#[tpd]`
- Table->tagged enums with one struct each (`#[tpd(tag="toml-key")]` on enum, `#[tpd]` on inner structs, `#[tpd(nested)]` on fields
- Vectors of the above
- `IndexMaps<FromString, One-of-the-above>`




### How this works:

`#[tpd]` writes a `PartialT` for every struct T you apply it on,
and implementations to go from `toml_edit` types to your `PartialT`, as well
as a conversion to turn complete `PartialT` back into T, all using the visitor pattern.

The `PartialT` consists of all the same fields as your `T`, wrapped in `TomlValue` - which records the
deserialization state and where in the TOML document it was.

`T::tpd_from_toml` will then give you an Ok(T) or an Err with a list of errors and the partial
state (If parsing succeeded. Otherwise you get just a `DeserError::ParsingFailure`. See [`DeserError`].


The errors contain information about what went wrong, and can turn themselves into pretty error messages
as in the example above.

### Why not just serde

Serde is great. If it works for you, good, use it!.
It's got way more battle hardening than this.

This offers:
- partial deserialization
- pretty, highly targeted error messages
- multiple errors at once
- the ability to access the partial parent during verification.
- optionally snake-case (anyCase, actually) and upper/lower case insensitivity.



### Usage Variations

#### No custom validation

Use `#[tpd(no_verify)]` on your struct, and skip writing a [`VerifyIn`] implementation.


#### Nested structs

When you want to represent `NestedT` as `PartialNestedT` inside your struct T,
tag them with `#[tpd(nested)]`


#### Enums

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

#[tpd(tag="kind")]
#[derive(Debug)]
enum EitherOne {
    KindA(InnerA),
    KindB(InnerB),
}
```


### Aliases

You can supply alias names on any field using `#[tpd(alias="another-name")]`.
This can be repeated.

Note the section below on casing for the common case of case- or code-case-insensitivity


### Casing

By default, field names and enum variants are matched strictly.

`tpd_from_toml`'s `FieldMatchMode` argument allows you to change this to case-insensitive (upper/lower only) or
code-case-insensitive (allowing '`snake_case`', 'camelCase', 'kebab-case', etc. to match each other).


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
you can use the [`impl_visitor`] macro:


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


### "the trait bound `InnerStruct:Visitor` is not satisfied"

You're missing the `#[tpd(nested)]` on your field definition,
the Visitor trait is implemented by `#[tpd]` for the `PartialT` 
not the `T`






