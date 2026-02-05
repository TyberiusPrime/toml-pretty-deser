# toml-pretty-deser

Deserialize TOML with pretty error messages.

Usage:

Deserialize your configuration structs from TOML with beautiful error message,
and keeping whatever could be understood around in case of errors:

```toml
        a = 5
        b = 10
        c = 3
```

becomes

```
  ╭─example-table.toml
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
  ┆             ╰─ See c
──╯
Hint: For example, set a = 33, b=66, c=0
```

Using this client Rust code:

```rust
use toml_pretty_deser::prelude::*;
#[tpd_make_partial(false)]
struct ShowOffTwoValueErrors {
    a: i64,
    b: i64,
    c: i64,
}

impl VerifyFromToml for PartialShowOffTwoValueErrors {
    fn verify(self, helper: &mut TomlHelper<'_>) -> Self
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
                    (self.c.span(), "See c".to_string()),
                ];
                helper.add_err_by_spans(spans, "For example, set a = 33, b=66, c=0")
            }
        }
        self
    }
}
let toml = "
        a = 5
        b = 10
        c = 3
        ";

    let result = deserialize::<PartialShowOffTwoValueErrors, ShowOffTwoValueErrors>(toml);
    if let Err(DeserError::DeserFailure(errors, _partial)) = result {
        println!("{}", errors[0].pretty("example-table.toml"));
    }
```

== How this works:

`#[tpd_make_partial]` writes a PartialT for every struct T you apply it on,
and implementations to go from toml_edit types to your PartialT, as well
as a conversion to turn complete PartialT back into T.

`deserialize` will thus give you an Ok(T) or a `Err(list_of_errors: Vec<HydratedAnnotatedError, partial: PartialT)`

The hydrated errors contain information about what went wrong, and can turn themselves into pretty error messages
as above.

== Why not just serde

Serde is great. If it works for you, good, use it!.

But I want pretty error messages, more than one error reported at once,
pinpoint accuracy in the errors and the ability to continue on with partially
read configurations.