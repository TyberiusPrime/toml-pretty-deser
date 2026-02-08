//! Test that having multiple #[tpd_absorb_remaining] fields is a compile error
use indexmap::IndexMap;
use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug)]
struct MultipleAbsorb {
    name: String,
    #[tpd_absorb_remaining]
    extra1: IndexMap<String, toml_edit::Item>,
    #[tpd_absorb_remaining]
    extra2: IndexMap<String, toml_edit::Item>,
}

fn main() {}
