use toml_pretty_deser::prelude::*;

#[tpd(root, no_verify)]
struct MyStruct {
    key: u8}

#[tpd]
#[derive(Debug)]
enum TN {
    TagA (MyStruct),
    TagB (MyStruct)
}

fn main() {
}
