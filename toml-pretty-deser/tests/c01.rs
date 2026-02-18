use toml_pretty_deser::prelude::*;

/// Tests that are not straight ports from 'manual impl' a01 to 'macro impl' b01
///
#[tpd(root)]
#[derive(Debug)]
#[allow(dead_code)]
struct Outer {
    value: u8,
}

impl VerifyIn<Root> for PartialOuter {}

#[test]
fn test_deser_error_std_error_impl() {
    use std::error::Error;

    // ParsingFailure: Display mentions "TOML parse error", source() is Some
    let result: Result<Outer, _> = Outer::tpd_from_toml(
        "this is not valid toml ===",
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    assert!(result.is_err());
    if let Err(e) = result {
        assert!(
            e.to_string().contains("TOML parse error"),
            "unexpected Display: {e}"
        );
        assert!(e.source().is_some(), "ParsingFailure should have a source");
    }

    // DeserFailure: Display mentions "deserialization failed", source() is None
    let result: Result<Outer, _> = Outer::tpd_from_toml(
        "# all required fields missing",
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    assert!(result.is_err());
    if let Err(e) = result {
        assert!(
            e.to_string().contains("deserialization failed"),
            "unexpected Display: {e}"
        );
        assert!(e.source().is_none(), "DeserFailure should have no source");
    }
}
