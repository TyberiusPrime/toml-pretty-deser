# toml-pretty-deser-macros

[Documentation](https://docs.rs/toml-pretty-deser-macros) | [crates.io](https://crates.io/crates/toml-pretty-deser-macros)

Macro crate for [toml-pretty-deser](https://docs.rs/toml-pretty-deser) — provides derive macros for TOML deserialization with rich, helpful error messages.

## Overview

This crate provides procedural macros that work with the `toml-pretty-deser` library to deserialize TOML config files while providing detailed, user-friendly error messages with suggestions, spans, and context.

## Features

- **`#[tpd_make_partial]`** — Tag structs for deserialization with partial results and validation
- **`#[tpd_make_enum]`** — Enable unit enums for use in configs
- **`#[tpd_make_tagged_enum("key")]`** — Support tagged enums (sum types) with a discriminator key

## Example

```rust
use toml_pretty_deser_macros::{tpd_make_enum, tpd_make_partial};

#[tpd_make_enum]
#[derive(Debug, Clone)]
enum Color {
    Red,
    Green,
    Blue,
}

#[tpd_make_partial]
struct Config {
    color: Color,
    name: String,
}
```

## License

See the main [toml-pretty-deser](https://github.com/anomalyco/toml-pretty-deser) repository for license information.
