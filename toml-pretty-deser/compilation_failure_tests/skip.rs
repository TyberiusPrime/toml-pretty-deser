use toml_pretty_deser::prelude::*;

#[tpd(root)]
#[derive(Debug)]
struct Skipped {
    a_u8: u8,
    #[tpd(skip)]
    skipped: u8,
}

impl VerifyIn<TPDRoot> for PartialSkipped {}

fn main() {
    let toml = "";
    let result: Result<_, _> =
        Skipped::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::SingleOk);
    dbg!(&result);
    assert!(!result.is_ok());
    if let Err(e @ DeserError::DeserFailure(..)) = result {
        let partial = e.partial().unwrap().value.as_ref().unwrap();
        // Skipped fields now exist in the partial as Option<T>, not TomlValue<T>
        // This test verifies they don't have .value (which would be TomlValue's field)
        assert_eq!(partial.skipped.value, None);
    }
}
