use indexmap::IndexMap;

use crate::{TomlValue, TomlValueState, ValidationFailure, Visitor};

pub trait FailableKeys {
    fn verify_keys<F>(&mut self, callback: F)
    where
        F: Fn(&str) -> Result<(), ValidationFailure>;
}

/// so we have a place to record the key failures
#[derive(Debug)]
pub struct MapAndKeys<K, V> {
    pub map: IndexMap<K, TomlValue<V>>,
    pub keys: Vec<TomlValue<String>>,
}

impl<K, V> FailableKeys for TomlValue<MapAndKeys<K, V>> {
    fn verify_keys<F>(&mut  self, callback: F)
    where
        F: Fn(&str) -> Result<(), ValidationFailure>,
    {
        if let Some(value) = self.as_mut() {
            for key in &mut value.keys {
                if let Some(str_key) = key.as_ref() {
                    match callback(&str_key[..]) {
                        Ok(_) => {}
                        Err(err) => {
                            key.state = TomlValueState::ValidationFailed {
                                span: key.span(),
                                message: err.message,
                            };
                            key.help = err.help;
                        }
                    }
                }
            }
        }
    }
}
