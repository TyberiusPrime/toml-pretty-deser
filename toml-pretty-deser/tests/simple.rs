use toml_pretty_deser::prelude::*;

#[tpd_make_partial(true)]
#[derive(Debug)]
struct Output {
    a_u8: u8,
}

#[test]
fn test_happy_path() {
    let toml = "
            a_u8 = 255
        ";

    let result: Result<_, _> = deserialize::<PartialOutput, Output>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a_u8, 255);
    }
}