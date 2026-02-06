use toml_pretty_deser::prelude::*;

#[derive(Debug, PartialEq, Eq)]
struct MyUnit(String);

#[tpd]
#[derive(Debug)]
struct WithUnit {
    a: MyUnit,
}

#[test]
#[allow(clippy::float_cmp)]
fn test_happy_path() {
    let toml = "
        a = 'hello'
        ";

    let result: Result<_, _> = deserialize::<PartialWithUnit, WithUnit>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a, MyUnit("hello".to_string()));
    }
}
