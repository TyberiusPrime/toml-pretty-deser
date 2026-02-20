#![doc=include_str!("../README.md")]

pub mod prelude;

mod error;
mod value;
mod collector;
mod table_helper;
mod traits;
mod adapt;
mod deser;
mod case;
mod tablelike;
mod visitors;

pub use error::{AnnotatedError, DeserError, HydratedAnnotatedError, SpannedMessage};
pub use value::{TomlValue, TomlValueState, UnknownKey, VecMode};
pub use collector::TomlCollector;
pub use table_helper::TomlHelper;
pub use traits::{TPDRoot, ValidationFailure, VerifyIn, VerifyVisitor, Visitor};
pub use adapt::{MustAdapt, MustAdaptHelper, MustAdaptNested};
pub use deser::deserialize_toml;
pub use case::{FieldMatchMode, suggest_alternatives};
pub use tablelike::{AsTableLikePlus, TableLikePlus};
