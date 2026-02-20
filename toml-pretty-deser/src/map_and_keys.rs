use indexmap::IndexMap;

use crate::{TomlValue, TomlValueState, ValidationFailure};

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

impl<K: std::hash::Hash + Eq, V> MapAndKeys<K, V> {
    /// Apply a function to each value in the map, producing a new `MapAndKeys` with the results.
    /// Used by the `#[tpd(with)]` macro for `IndexMap<K, V>` fields.
    pub fn map_values<W, F>(self, f: F) -> MapAndKeys<K, W>
    where
        F: Fn(TomlValue<V>) -> TomlValue<W>,
    {
        MapAndKeys {
            map: self.map.into_iter().map(|(k, v)| (k, f(v))).collect(),
            keys: self.keys,
        }
    }
}

impl<K, V> FailableKeys for TomlValue<MapAndKeys<K, V>> {
    fn verify_keys<F>(&mut self, callback: F)
    where
        F: Fn(&str) -> Result<(), ValidationFailure>,
    {
        let mut any_failed = false;
        if let Some(value) = self.as_mut() {
            for key in &mut value.keys {
                if let Some(str_key) = key.as_ref() {
                    match callback(&str_key[..]) {
                        Ok(_) => {}
                        Err(err) => {
                            key.state = TomlValueState::ValidationFailed {
                                message: err.message,
                            };
                            key.help = err.help;
                            any_failed = true;
                        }
                    }
                }
            }
        }
        if any_failed {
            self.state = TomlValueState::Nested {};
        }
    }
}
