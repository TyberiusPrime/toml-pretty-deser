use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug, PartialEq, Eq)]
struct MyUnit(String);

#[tpd]
#[derive(Debug, PartialEq, Eq)]
struct MyInt(i8);

#[tpd]
#[derive(Debug)]
struct WithUnit {
    a: MyUnit,
    b: MyInt,
}

#[test]
#[allow(clippy::float_cmp)]
fn test_happy_path() {
    let toml = "
        a = 'hello'
         b = 120
        ";

    let result: Result<_, _> = deserialize::<PartialWithUnit, WithUnit>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a, MyUnit("hello".to_string()));
        assert_eq!(output.b, MyInt(120));
    }
}
