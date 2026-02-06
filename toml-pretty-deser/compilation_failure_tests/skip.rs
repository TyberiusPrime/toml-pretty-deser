use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug)]
struct Skipped {
    a_u8: u8,
    #[tpd_skip]
    skipped: u8,
}

fn main() {
    let toml = "";
    let result: Result<_, _> = deserialize::<PartialSkipped, Skipped>(toml);
    dbg!(&result);
    assert!(!result.is_ok());
    if let Err(DeserError::DeserFailure(_errors, partial)) = result {
        assert_eq!(partial.skipped.value, None);
    }
}
