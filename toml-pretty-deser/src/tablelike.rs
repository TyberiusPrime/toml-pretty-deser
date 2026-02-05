///
/// Reimplement TableLike until the PR in toml_edit lands
use toml_edit::{InlineTable, Item, Iter, Key, Table, TableLike};

/// Get a TableLikePlus from an Item if it is either a Table or InlineTable
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
    fn get<'s>(&'s self, key: &str) -> Option<&'s Item>;
    fn key(&self, key: &str) -> Option<&'_ Key>;
    // return the location within the Document
    fn span(&self) -> Option<std::ops::Range<usize>>;
}

#[mutants::skip]
impl TableLikePlus for Table {
    fn iter(&self) -> Iter<'_> {
        self.iter()
    }
    fn get<'s>(&'s self, key: &str) -> Option<&'s Item> {
        self.get(key)
    }

    fn key(&self, key: &str) -> Option<&'_ Key> {
        self.key(key)
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

    fn get<'s>(&'s self, key: &str) -> Option<&'s Item> {
        TableLike::get(self, key)
    }

    fn key(&self, key: &str) -> Option<&'_ Key> {
        self.key(key)
    }

    fn span(&self) -> Option<std::ops::Range<usize>> {
        self.span()
    }
}
