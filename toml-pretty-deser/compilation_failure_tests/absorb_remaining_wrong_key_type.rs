//! Test that #[tpd_absorb_remaining] on IndexMap<i32, T> is a compile error
use indexmap::IndexMap;
use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug)]
struct WrongKeyType {
    name: String,
    #[tpd(absorb_remaining)]
    extra: IndexMap<i32, String>,
}

fn main() {}
