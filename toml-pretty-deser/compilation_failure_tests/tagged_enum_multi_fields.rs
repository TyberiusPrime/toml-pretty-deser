use toml_pretty_deser::prelude::*;

#[tpd(root, no_verify)]
struct MyStruct {
    key: u8}

#[tpd(tag="kind")]
#[derive(Debug)]
enum TN {
    TagA (MyStruct,MyStruct),
    TagB (MyStruct)
}

fn main() {
}
