//! Test that #[tpd_absorb_remaining] on Vec<T> is a compile error
use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug)]
struct WrongTypeVec {
    name: String,
    #[tpd_absorb_remaining]
    extra: Vec<String>,
}

fn main() {}
