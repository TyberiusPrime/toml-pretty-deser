#![doc=include_str!("../README.md")]

pub mod prelude;

mod adapt;
mod case;
mod collector;
mod deser;
mod error;
mod map_and_keys;
mod table_helper;
mod tablelike;
mod traits;
mod value;
mod visitors;

pub use adapt::{MustAdapt, MustAdaptHelper, MustAdaptNested};
pub use case::{FieldMatchMode, format_quoted_list, suggest_alternatives};
pub use collector::TomlCollector;
pub use deser::deserialize_toml;
pub use error::{AnnotatedError, DeserError, HydratedAnnotatedError, SpannedMessage};
pub use map_and_keys::{FailableKeys, MapAndKeys};
pub use table_helper::TomlHelper;
pub use tablelike::{AsTableLikePlus, TableLikePlus};
pub use traits::{TPDRoot, ValidationFailure, VerifyIn, VerifyVisitor, Visitor};
pub use value::{TomlOr, TomlValue, TomlValueState, UnknownKey, VecMode};
