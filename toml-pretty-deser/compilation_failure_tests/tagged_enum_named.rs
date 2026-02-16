use toml_pretty_deser::prelude::*;

#[tpd(tag="kind")]
#[derive(Debug)]
enum TN {
    TagA { field: u8},
    TagB { field: u8}
}

fn main() {
}
