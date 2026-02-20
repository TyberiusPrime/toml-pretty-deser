
use toml_pretty_deser::{TomlCollector, Visitor, prelude::*};
#[tpd(root)]
#[derive(Debug)]
struct NestedAndAdapt {
    #[tpd(nested, with = "adapt_inner")]
    nested: Inner,
}

#[tpd]
#[derive(Debug)]
struct Inner {
    a_u8: i64,
}

impl VerifyIn<TPDRoot> for PartialNestedAndAdapt {}

fn adapt_inner(mut input: TomlValue<toml_edit::Item>) -> TomlValue<PartialInner> {
    input.try_map(|value| -> Result<PartialInner, ValidationFailure> {
        match value.as_integer() {
            Some(i) => Ok(PartialInner {
                a_u8: TomlValue::new_ok(i, value.span().unwrap_or(0..0)),
            }),
            None => {
                let mut helper = TomlHelper::from_item(value, TomlCollector::default());
                let tv = PartialInner::fill_from_toml(&mut helper);
                if let Some(tv) = tv.into_inner() {
                    Ok(tv)
                } else {
                    Err(ValidationFailure::new(
                        "Could not decode as either u8 or Inner",
                        None,
                    ))
                }
            }
        }
    })
}

fn main() {
    let toml = "
            nested = 34
";
    let result: Result<_, _> =
        NestedAndAdapt::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::SingleOk);
    assert!(result.is_ok());
    if let Ok(result) = result {
        assert_eq!(result.nested.a_u8, 34);
    }
    let toml = "
[nested]
    a_u8 = 43
";
    let result: Result<_, _> =
        NestedAndAdapt::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::SingleOk);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(result) = result {
        assert_eq!(result.nested.a_u8, 43);
    }
}
