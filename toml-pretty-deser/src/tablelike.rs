///
/// Reimplement TableLike until the PR in toml_edit lands
use toml_edit::{Entry, InlineTable, Item, Iter, IterMut, Key, KeyMut, Table, TableLike, Value};

pub trait AsTableLikePlus {
    /// Casts `self` to either a table or an inline table.
    fn as_table_like_plus(&self) -> Option<&dyn TableLikePlus>;
}

impl AsTableLikePlus for toml_edit::Item {
    fn as_table_like_plus(&self) -> Option<&dyn TableLikePlus> {
        self.as_table()
            .map(|t| t as &dyn TableLikePlus)
            .or_else(|| self.as_inline_table().map(|t| t as &dyn TableLikePlus))
    }
}


/// This trait represents either a `Table`, or an `InlineTable`.
#[mutants::skip]
pub trait TableLikePlus {
    /// Returns an iterator over key/value pairs.
    fn iter(&self) -> Iter<'_>;
    /// Returns an mutable iterator over all key/value pairs, including empty.
    fn iter_mut(&mut self) -> IterMut<'_>;
    /// Returns the number of nonempty items.
    fn len(&self) -> usize {
        self.iter().filter(|&(_, v)| !v.is_none()).count()
    }
    /// Returns true if the table is empty.
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
    /// Clears the table, removing all key-value pairs. Keeps the allocated memory for reuse.
    fn clear(&mut self);
    /// Gets the given key's corresponding entry in the Table for in-place manipulation.
    fn entry<'a>(&'a mut self, key: &str) -> Entry<'a>;
    /// Gets the given key's corresponding entry in the Table for in-place manipulation.
    fn entry_format<'a>(&'a mut self, key: &Key) -> Entry<'a>;
    /// Returns an optional reference to an item given the key.
    fn get<'s>(&'s self, key: &str) -> Option<&'s Item>;
    /// Returns an optional mutable reference to an item given the key.
    fn get_mut<'s>(&'s mut self, key: &str) -> Option<&'s mut Item>;
    /// Return references to the key-value pair stored for key, if it is present, else None.
    fn get_key_value<'a>(&'a self, key: &str) -> Option<(&'a Key, &'a Item)>;
    /// Return mutable references to the key-value pair stored for key, if it is present, else None.
    fn get_key_value_mut<'a>(&'a mut self, key: &str) -> Option<(KeyMut<'a>, &'a mut Item)>;
    /// Returns true if the table contains an item with the given key.
    fn contains_key(&self, key: &str) -> bool;
    /// Inserts a key-value pair into the map.
    fn insert(&mut self, key: &str, value: Item) -> Option<Item>;
    /// Removes an item given the key.
    fn remove(&mut self, key: &str) -> Option<Item>;

    /// Get key/values for values that are visually children of this table
    ///
    /// For example, this will return dotted keys
    fn get_values(&self) -> Vec<(Vec<&Key>, &Value)>;

    /// Auto formats the table.
    fn fmt(&mut self);
    /// Sorts [Key]/[Value]-pairs of the table
    ///
    /// <div class="warning">
    ///
    /// This sorts the syntactic table (everything under the `[header]`) and not the logical map of
    /// key-value pairs.
    /// This does not affect the order of [sub-tables][Table] or [sub-arrays][crate::ArrayOfTables].
    /// This is not recursive.
    ///
    /// </div>
    fn sort_values(&mut self);
    /// Change this table's dotted status
    fn set_dotted(&mut self, yes: bool);
    /// Check if this is a wrapper for dotted keys, rather than a standard table
    fn is_dotted(&self) -> bool;

    /// Returns an accessor to a key's formatting
    fn key(&self, key: &str) -> Option<&'_ Key>;
    /// Returns an accessor to a key's formatting
    fn key_mut(&mut self, key: &str) -> Option<KeyMut<'_>>;
    ////// The location within the original document
    ///
    /// This generally requires a [`Document`][crate::Document].
    fn span(&self) -> Option<std::ops::Range<usize>>;
}

#[mutants::skip]
impl TableLikePlus for Table {
    fn iter(&self) -> Iter<'_> {
        self.iter()
    }
    fn iter_mut(&mut self) -> IterMut<'_> {
        self.iter_mut()
    }
    fn clear(&mut self) {
        self.clear();
    }
    fn entry<'a>(&'a mut self, key: &str) -> Entry<'a> {
        self.entry(key)
    }
    fn entry_format<'a>(&'a mut self, key: &Key) -> Entry<'a> {
        self.entry_format(key)
    }
    fn get<'s>(&'s self, key: &str) -> Option<&'s Item> {
        self.get(key)
    }
    fn get_mut<'s>(&'s mut self, key: &str) -> Option<&'s mut Item> {
        self.get_mut(key)
    }
    fn get_key_value<'a>(&'a self, key: &str) -> Option<(&'a Key, &'a Item)> {
        self.get_key_value(key)
    }
    fn get_key_value_mut<'a>(&'a mut self, key: &str) -> Option<(KeyMut<'a>, &'a mut Item)> {
        self.get_key_value_mut(key)
    }
    fn contains_key(&self, key: &str) -> bool {
        self.contains_key(key)
    }
    fn insert(&mut self, key: &str, value: Item) -> Option<Item> {
        self.insert(key, value)
    }
    fn remove(&mut self, key: &str) -> Option<Item> {
        self.remove(key)
    }

    fn get_values(&self) -> Vec<(Vec<&Key>, &Value)> {
        self.get_values()
    }
    fn fmt(&mut self) {
        self.fmt();
    }
    fn sort_values(&mut self) {
        self.sort_values();
    }
    fn is_dotted(&self) -> bool {
        self.is_dotted()
    }
    fn set_dotted(&mut self, yes: bool) {
        self.set_dotted(yes);
    }

    fn key(&self, key: &str) -> Option<&'_ Key> {
        self.key(key)
    }
    fn key_mut(&mut self, key: &str) -> Option<KeyMut<'_>> {
        self.key_mut(key)
    }

    fn span(&self) -> Option<std::ops::Range<usize>> {
        self.span()
    }
}

#[mutants::skip]
impl TableLikePlus for InlineTable {
    fn iter(&self) -> Iter<'_> {
        TableLike::iter(self)
    }
    fn iter_mut(&mut self) -> IterMut<'_> {
        TableLike::iter_mut(self)
    }
    fn clear(&mut self) {
        self.clear();
    }
    fn entry<'a>(&'a mut self, key: &str) -> Entry<'a> {
        TableLike::entry(self, key)
    }
    fn entry_format<'a>(&'a mut self, key: &Key) -> Entry<'a> {
        TableLike::entry_format(self, key)
    }
    fn get<'s>(&'s self, key: &str) -> Option<&'s Item> {
        TableLike::get(self, key)
    }
    fn get_mut<'s>(&'s mut self, key: &str) -> Option<&'s mut Item> {
        TableLike::get_mut(self, key)
    }
    fn get_key_value<'a>(&'a self, key: &str) -> Option<(&'a Key, &'a Item)> {
        self.get_key_value(key)
    }
    fn get_key_value_mut<'a>(&'a mut self, key: &str) -> Option<(KeyMut<'a>, &'a mut Item)> {
        self.get_key_value_mut(key)
    }
    fn contains_key(&self, key: &str) -> bool {
        self.contains_key(key)
    }
    fn insert(&mut self, key: &str, value: Item) -> Option<Item> {
        self.insert(key, value.into_value().unwrap())
            .map(Item::Value)
    }
    fn remove(&mut self, key: &str) -> Option<Item> {
        self.remove(key).map(Item::Value)
    }

    fn get_values(&self) -> Vec<(Vec<&Key>, &Value)> {
        self.get_values()
    }
    fn fmt(&mut self) {
        self.fmt();
    }
    fn sort_values(&mut self) {
        self.sort_values();
    }
    fn set_dotted(&mut self, yes: bool) {
        self.set_dotted(yes);
    }
    fn is_dotted(&self) -> bool {
        self.is_dotted()
    }

    fn key(&self, key: &str) -> Option<&'_ Key> {
        self.key(key)
    }
    fn key_mut(&mut self, key: &str) -> Option<KeyMut<'_>> {
        self.key_mut(key)
    }

    fn span(&self) -> Option<std::ops::Range<usize>> {
        self.span()
    }
}
