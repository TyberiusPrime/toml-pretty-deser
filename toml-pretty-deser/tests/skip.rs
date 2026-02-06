use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug)]
struct Skipped {
    a_u8: u8,
    #[tpd_skip]
    skipped: u8,
}

#[test]
fn test_skipped() {
    let toml = "a_u8 = 123";
    let result: Result<_, _> = deserialize::<PartialSkipped, Skipped>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a_u8, 123);
        assert_eq!(output.skipped, Default::default());
    }
}

#[test]
fn test_skipped_partial_has_no_skipped() {
    let toml = "a_u8 = 123";
    let result: Result<_, _> = deserialize::<PartialSkipped, Skipped>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a_u8, 123);
        assert_eq!(output.skipped, 0);
    }
}
