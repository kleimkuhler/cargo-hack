#![deny(missing_docs)]
#![allow(rust_2018_idioms, single_use_lifetimes, unreachable_pub)]

//! # `toml_edit`
//!
//! This crate allows you to parse and modify toml
//! documents, while preserving comments, spaces* and
//! relative order* or items.
//!
//! It is primarily tailored to the needs of [cargo-edit](https://github.com/killercup/cargo-edit/).
//!
//! # Example
//!
//! ```rust
//! use toml_edit::{Document, value};
//!
//! let toml = r#"
//! "hello" = 'toml!' # comment
//! ['a'.b]
//! "#;
//! let mut doc = toml.parse::<Document>().expect("invalid doc");
//! assert_eq!(doc.to_string(), toml);
//! // let's add a new key/value pair inside a.b: c = {d = "hello"}
//! doc["a"]["b"]["c"]["d"] = value("hello");
//! // autoformat inline table a.b.c: { d = "hello" }
//! doc["a"]["b"]["c"].as_inline_table_mut().map(|t| t.fmt());
//! let expected = r#"
//! "hello" = 'toml!' # comment
//! ['a'.b]
//! c = { d = "hello" }
//! "#;
//! assert_eq!(doc.to_string(), expected);
//! ```
//!
//! ## Limitations
//!
//! Things it does not preserve:
//!
//! * Different quotes and spaces around the same table key, e.g.
//!
//! ```text
//! [ 'a'. b]
//! [ "a"  .c]
//! [a.d]
//! ```
//!
//! will be represented as (spaces are removed, the first encountered quote type is used)
//!
//! ```text
//! ['a'.b]
//! ['a'.c]
//! ['a'.d]
//! ```
//!
//! * Children tables before parent table (tables are reordered by default, see [test]).
//! * Scattered array of tables (tables are reordered by default, see [test]).
//!
//! The reason behind the first limitation is that `Table` does not store its header,
//! allowing us to safely swap two tables
//! (we store a mapping in each table: child key -> child table).
//!
//! This last two limitations allow us to represent a toml document as a tree-like data structure,
//! which enables easier implementation of editing operations
//! and an easy to use and type-safe API. If you care about the above two cases,
//! you can use `Document::to_string_in_original_order()` to reconstruct tables in their original order.
//!
//! [test]: https://github.com/ordian/toml_edit/blob/f09bd5d075fdb7d2ef8d9bb3270a34506c276753/tests/test_valid.rs#L84

macro_rules! try_parse {
    ($s:expr, $p:expr) => {{
        use combine::Parser;
        let result = $p.easy_parse(State::new($s));
        match result {
            Ok((_, ref rest)) if !rest.input.is_empty() => {
                Err(TomlError::from_unparsed(rest.positioner, $s))
            }
            Ok((s, _)) => Ok(s),
            Err(e) => Err(TomlError::new(e.into(), $s)),
        }
    }};
}

pub(crate) mod array_of_tables {
    use super::table::{Item, Table};
    /// Type representing a TOML array of tables
    #[derive(Clone, Debug, Default)]
    pub struct ArrayOfTables {
        pub(crate) values: Vec<Item>,
    }
    /// An iterator type over `ArrayOfTables`'s values.
    type ArrayOfTablesIter<'a> = Box<dyn Iterator<Item = &'a Table> + 'a>;
    type ArrayOfTablesIterMut<'a> = Box<dyn Iterator<Item = &'a mut Table> + 'a>;
    impl ArrayOfTables {
        /// Creates an empty array of tables.
        pub fn new() -> Self {
            Default::default()
        }
        /// Returns an iterator over tables.
        pub fn iter(&self) -> ArrayOfTablesIter<'_> {
            Box::new(self.values.iter().filter_map(Item::as_table))
        }
        /// Returns an iterator over tables.
        pub fn iter_mut(&mut self) -> ArrayOfTablesIterMut<'_> {
            Box::new(self.values.iter_mut().filter_map(Item::as_table_mut))
        }
        /// Returns an optional reference to the table.
        pub fn get(&self, index: usize) -> Option<&Table> {
            self.values.get(index).and_then(Item::as_table)
        }
        /// Returns an optional mutable reference to the table.
        pub fn get_mut(&mut self, index: usize) -> Option<&mut Table> {
            self.values.get_mut(index).and_then(Item::as_table_mut)
        }
        /// Appends a table to the array.
        pub fn append(&mut self, table: Table) -> &mut Table {
            self.values.push(Item::Table(table));
            let i = self.len() - 1;
            self.get_mut(i).unwrap()
        }
        /// Removes a table with the given index.
        pub fn remove(&mut self, index: usize) {
            self.values.remove(index);
        }
        /// Removes all the tables.
        pub fn clear(&mut self) {
            self.values.clear()
        }
        /// Returns the length of the underlying Vec.
        /// To get the actual number of items use `a.iter().count()`.
        pub fn len(&self) -> usize {
            self.values.len()
        }
        /// Returns true iff `self.len() == 0`.
        pub fn is_empty(&self) -> bool {
            self.len() == 0
        }
    }
}
pub(crate) mod decor {
    /// A value together with its `to_string` representation,
    /// including surrounding it whitespaces and comments.
    #[derive(Eq, PartialEq, Clone, Debug, Hash)]
    pub struct Formatted<T> {
        value: T,
        pub(crate) repr: Repr,
    }
    #[derive(Eq, PartialEq, Clone, Debug, Hash)]
    pub(crate) struct Repr {
        pub decor: Decor,
        pub raw_value: String,
    }
    /// A prefix and suffix,
    /// including comments, whitespaces and newlines.
    #[derive(Eq, PartialEq, Clone, Default, Debug, Hash)]
    pub struct Decor {
        pub(crate) prefix: String,
        pub(crate) suffix: String,
    }
    impl Decor {
        /// Creates a new decor from the given prefix and suffix.
        pub fn new<S: Into<String>>(prefix: S, suffix: S) -> Self {
            Self { prefix: prefix.into(), suffix: suffix.into() }
        }
        /// Get the prefix.
        pub fn prefix(&self) -> &str {
            &self.prefix
        }
        /// Get the suffix.
        pub fn suffix(&self) -> &str {
            &self.suffix
        }
    }
    impl Repr {
        pub fn new<S: Into<String>>(prefix: S, value: S, suffix: S) -> Self {
            Repr { decor: Decor::new(prefix, suffix), raw_value: value.into() }
        }
    }
    impl<T> Formatted<T> {
        pub fn raw(&self) -> &str {
            &self.repr.raw_value
        }
        pub fn prefix(&self) -> &str {
            &self.repr.decor.prefix
        }
        pub fn suffix(&self) -> &str {
            &self.repr.decor.suffix
        }
        pub fn value(&self) -> &T {
            &self.value
        }
        pub(crate) fn new(v: T, repr: Repr) -> Self {
            Self { value: v, repr }
        }
    }
}
pub(crate) mod display {
    use super::{
        decor::{Formatted, Repr},
        document::Document,
        table::{Item, Table},
        value::{Array, DateTime, InlineTable, Value},
    };
    use std::fmt::{Display, Formatter, Result, Write};
    impl Display for Repr {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            write!(f, "{}{}{}", self.decor.prefix, self.raw_value, self.decor.suffix)
        }
    }
    impl<T> Display for Formatted<T> {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            write!(f, "{}", self.repr)
        }
    }
    impl Display for DateTime {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            match *self {
                DateTime::OffsetDateTime(d) => write!(f, "{}", d),
                DateTime::LocalDateTime(d) => write!(f, "{}", d),
                DateTime::LocalDate(d) => write!(f, "{}", d),
                DateTime::LocalTime(d) => write!(f, "{}", d),
            }
        }
    }
    impl Display for Value {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            match *self {
                Value::Integer(ref repr) => write!(f, "{}", repr),
                Value::String(ref repr) => write!(f, "{}", repr),
                Value::Float(ref repr) => write!(f, "{}", repr),
                Value::Boolean(ref repr) => write!(f, "{}", repr),
                Value::DateTime(ref repr) => write!(f, "{}", repr),
                Value::Array(ref array) => write!(f, "{}", array),
                Value::InlineTable(ref table) => write!(f, "{}", table),
            }
        }
    }
    impl Display for Array {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            write!(f, "{}[", self.decor.prefix)?;
            join(f, self.iter(), ",")?;
            if self.trailing_comma {
                write!(f, ",")?;
            }
            write!(f, "{}", self.trailing)?;
            write!(f, "]{}", self.decor.suffix)
        }
    }
    impl Display for InlineTable {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            write!(f, "{}{{", self.decor.prefix)?;
            write!(f, "{}", self.preamble)?;
            for (i, (key, value)) in self
                .items
                .iter()
                .filter(|&(_, kv)| kv.value.is_value())
                .map(|(_, kv)| (&kv.key, kv.value.as_value().unwrap()))
                .enumerate()
            {
                if i > 0 {
                    write!(f, ",")?;
                }
                write!(f, "{}={}", key, value)?;
            }
            write!(f, "}}{}", self.decor.suffix)
        }
    }
    impl Table {
        fn visit_nested_tables<'t, F>(
            &'t self,
            path: &mut Vec<&'t str>,
            is_array_of_tables: bool,
            callback: &mut F,
        ) -> Result
        where
            F: FnMut(&Table, &Vec<&'t str>, bool) -> Result,
        {
            callback(self, path, is_array_of_tables)?;
            for kv in self.items.values() {
                match kv.value {
                    Item::Table(ref t) => {
                        path.push(&kv.key.raw_value);
                        t.visit_nested_tables(path, false, callback)?;
                        path.pop();
                    }
                    Item::ArrayOfTables(ref a) => {
                        for t in a.iter() {
                            path.push(&kv.key.raw_value);
                            t.visit_nested_tables(path, true, callback)?;
                            path.pop();
                        }
                    }
                    _ => {}
                }
            }
            Ok(())
        }
    }
    fn visit_table(
        f: &mut dyn Write,
        table: &Table,
        path: &[&str],
        is_array_of_tables: bool,
    ) -> Result {
        if path.is_empty() {
            // don't print header for the root node
        } else if is_array_of_tables {
            write!(f, "{}[[", table.decor.prefix)?;
            write!(f, "{}", path.join("."))?;
            writeln!(f, "]]{}", table.decor.suffix)?;
        } else if !(table.implicit && table.values_len() == 0) {
            write!(f, "{}[", table.decor.prefix)?;
            write!(f, "{}", path.join("."))?;
            writeln!(f, "]{}", table.decor.suffix)?;
        }
        // print table body
        for kv in table.items.values() {
            if let Item::Value(ref value) = kv.value {
                writeln!(f, "{}={}", kv.key, value)?;
            }
        }
        Ok(())
    }

    impl Display for Table {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            let mut path = Vec::new();
            self.visit_nested_tables(
                &mut path,
                false,
                &mut (|t, path, is_array| visit_table(f, t, path, is_array)),
            )?;
            Ok(())
        }
    }
    impl Document {
        /// Returns a string representation of the TOML document, attempting to keep
        /// the table headers in their original order.
        pub fn to_string_in_original_order(&self) -> String {
            let mut string = String::new();
            let mut path = Vec::new();
            let mut last_position = 0;
            let mut tables = Vec::new();
            self.as_table()
                .visit_nested_tables(
                    &mut path,
                    false,
                    &mut (|t, p, is_array| {
                        if let Some(pos) = t.position {
                            last_position = pos;
                        }
                        let mut s = String::new();
                        visit_table(&mut s, t, p, is_array)?;
                        tables.push((last_position, s));
                        Ok(())
                    }),
                )
                .unwrap();
            tables.sort_by_key(|&(id, _)| id);
            for (_, table) in tables {
                string.push_str(&table);
            }
            string.push_str(&self.trailing);
            string
        }
    }
    impl Display for Document {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            write!(f, "{}", self.as_table())?;
            write!(f, "{}", self.trailing)
        }
    }
    fn join<D>(f: &mut Formatter<'_>, iter: impl Iterator<Item = D>, sep: &str) -> Result
    where
        D: Display,
    {
        for (i, v) in iter.enumerate() {
            if i > 0 {
                write!(f, "{}", sep)?;
            }
            write!(f, "{}", v)?;
        }
        Ok(())
    }
}
pub(crate) mod document {
    use super::{
        parser,
        table::{Item, Iter, IterMut, Table},
    };
    use std::str::FromStr;
    /// Type representing a TOML document
    #[derive(Debug, Clone)]
    pub struct Document {
        /// Root should always be `Item::Table`.
        pub root: Item,
        pub(crate) trailing: String,
    }
    impl Default for Document {
        fn default() -> Self {
            Self { root: Item::Table(Table::with_pos(Some(0))), trailing: Default::default() }
        }
    }
    impl Document {
        /// Creates an empty document
        pub fn new() -> Self {
            Default::default()
        }
        /// Returns a reference to the root table.
        pub fn as_table(&self) -> &Table {
            self.root.as_table().expect("root should always be a table")
        }
        /// Returns a mutable reference to the root table.
        pub fn as_table_mut(&mut self) -> &mut Table {
            self.root.as_table_mut().expect("root should always be a table")
        }
        /// Returns an iterator over the root table.
        pub fn iter(&self) -> Iter<'_> {
            self.root.as_table().expect("root should always be a table").iter()
        }
        /// Returns an iterator over the root table.
        pub fn iter_mut(&mut self) -> IterMut<'_> {
            self.root.as_table_mut().expect("root should always be a table").iter_mut()
        }
    }
    impl FromStr for Document {
        type Err = parser::TomlError;
        /// Parses a document from a &str
        fn from_str(s: &str) -> Result<Self, Self::Err> {
            parser::TomlParser::parse(s)
        }
    }
}
pub(crate) mod formatted {
    use super::{
        decor::{Decor, Formatted, Repr},
        key::Key,
        parser::{strings, TomlError},
        table::{Item, KeyValuePairs, TableKeyValue},
        value::{Array, DateTime, InlineTable, Value},
    };
    use combine::stream::state::State;
    use std::iter::FromIterator;
    pub(crate) fn decorate_array(array: &mut Array) {
        for (i, val) in array.values.iter_mut().filter_map(Item::as_value_mut).enumerate() {
            if i > 0 {
                decorate(val, " ", "");
            } else {
                decorate(val, "", "");
            }
        }
    }
    pub(crate) fn decorate_inline_table(table: &mut InlineTable) {
        let n = table.len();
        for (i, (key, value)) in table
            .items
            .iter_mut()
            .filter(|&(_, ref kv)| kv.value.is_value())
            .map(|(_, kv)| (&mut kv.key, kv.value.as_value_mut().unwrap()))
            .enumerate()
        {
            key.decor.prefix = String::from(" ");
            key.decor.suffix = String::from(" ");
            if i == n - 1 {
                decorate(value, " ", " ");
            } else {
                decorate(value, " ", "");
            }
        }
    }
    pub(crate) fn decorate(value: &mut Value, prefix: &str, suffix: &str) {
        let decor = match *value {
            Value::Integer(ref mut f) => &mut f.repr.decor,
            Value::String(ref mut f) => &mut f.repr.decor,
            Value::Float(ref mut f) => &mut f.repr.decor,
            Value::DateTime(ref mut f) => &mut f.repr.decor,
            Value::Boolean(ref mut f) => &mut f.repr.decor,
            Value::Array(ref mut a) => &mut a.decor,
            Value::InlineTable(ref mut t) => &mut t.decor,
        };
        decor.prefix = String::from(prefix);
        decor.suffix = String::from(suffix);
    }
    /// Sets the prefix and the suffix for value.
    /// # Example
    /// ```rust
    /// let mut v = toml_edit::Value::from(42);
    /// assert_eq!(&v.to_string(), "42");
    /// let d = toml_edit::decorated(v, " ", " ");
    /// assert_eq!(&d.to_string(), " 42 ");
    /// ```
    pub fn decorated(mut value: Value, prefix: &str, suffix: &str) -> Value {
        {
            decorate(&mut value, prefix, suffix);
        }
        value
    }
    pub(crate) fn value(mut val: Value, raw: &str) -> Value {
        match val {
            Value::Integer(ref mut f) => {
                f.repr.raw_value = String::from(raw);
            }
            Value::String(ref mut f) => {
                f.repr.raw_value = String::from(raw);
            }
            Value::Float(ref mut f) => {
                f.repr.raw_value = String::from(raw);
            }
            Value::DateTime(ref mut f) => {
                f.repr.raw_value = String::from(raw);
            }
            Value::Boolean(ref mut f) => {
                f.repr.raw_value = String::from(raw);
            }
            _ => {}
        };
        decorate(&mut val, "", "");
        val
    }
    pub(crate) fn to_key_value(key: &str, mut value: Value) -> TableKeyValue {
        decorate(&mut value, " ", "");
        to_table_key_value(key, Item::Value(value))
    }
    pub(crate) fn to_table_key_value(key: &str, value: Item) -> TableKeyValue {
        TableKeyValue { key: key_repr(key), value }
    }
    pub(crate) fn key_repr(raw: &str) -> Repr {
        Repr {
            decor: Decor { prefix: String::from(""), suffix: String::from(" ") },
            raw_value: raw.into(),
        }
    }
    impl From<i64> for Value {
        fn from(i: i64) -> Self {
            Value::Integer(Formatted::new(
                i,
                Repr::new("".to_string(), i.to_string(), "".to_string()),
            ))
        }
    }
    impl From<f64> for Value {
        fn from(f: f64) -> Self {
            Value::Float(Formatted::new(
                f,
                Repr::new("".to_string(), f.to_string(), "".to_string()),
            ))
        }
    }
    fn parse_string_guess_delimiters(s: &str) -> (String, String) {
        let basic = format!("\"{}\"", s);
        let literal = format!("'{}'", s);
        let ml_basic = format!("\"\"\"{}\"\"\"", s);
        let ml_literal = format!("'''{}'''", s);
        if let Ok(r) = try_parse!(s, strings::string()) {
            (r, s.into())
        } else if let Ok(r) = try_parse!(&basic[..], strings::basic_string()) {
            (r, basic)
        } else if let Ok(r) = try_parse!(&literal[..], strings::literal_string()) {
            (r.into(), literal.clone())
        } else if let Ok(r) = try_parse!(&ml_basic[..], strings::ml_basic_string()) {
            (r, ml_literal)
        } else {
            try_parse!(&ml_literal[..], strings::ml_literal_string())
                .map(|r| (r, ml_literal))
                .unwrap_or_else(|e| panic!("toml string parse error: {}, {}", e, s))
        }
    }
    impl<'b> From<&'b str> for Value {
        fn from(s: &'b str) -> Self {
            let (value, raw) = parse_string_guess_delimiters(s);
            Value::String(Formatted::new(value, Repr::new("".to_string(), raw, "".to_string())))
        }
    }
    impl From<String> for Value {
        fn from(s: String) -> Self {
            Value::from(s.as_ref())
        }
    }
    impl From<bool> for Value {
        fn from(b: bool) -> Self {
            Value::Boolean(Formatted::new(b, Repr::new("", if b { "true" } else { "false" }, "")))
        }
    }
    impl From<DateTime> for Value {
        fn from(d: DateTime) -> Self {
            let s = d.to_string();
            Value::DateTime(Formatted::new(d, Repr::new("".to_string(), s, "".to_string())))
        }
    }
    impl From<Array> for Value {
        fn from(array: Array) -> Self {
            Value::Array(array)
        }
    }
    impl From<InlineTable> for Value {
        fn from(table: InlineTable) -> Self {
            Value::InlineTable(table)
        }
    }
    impl<V: Into<Value>> FromIterator<V> for Value {
        fn from_iter<I>(iter: I) -> Self
        where
            I: IntoIterator<Item = V>,
        {
            let v = iter.into_iter().map(|a| Item::Value(a.into()));
            let mut array = Array { values: v.collect(), ..Default::default() };
            decorate_array(&mut array);
            Value::Array(array)
        }
    }
    pub(crate) fn to_key_value_pairs<'k, K, V, I>(iter: I) -> KeyValuePairs
    where
        K: Into<&'k Key>,
        V: Into<Value>,
        I: IntoIterator<Item = (K, V)>,
    {
        let v = iter.into_iter().map(|(a, b)| {
            let s: &Key = a.into();
            (s.get().into(), to_key_value(s.raw(), b.into()))
        });
        KeyValuePairs::from_iter(v)
    }
    impl<'k, K: Into<&'k Key>, V: Into<Value>> FromIterator<(K, V)> for Value {
        fn from_iter<I>(iter: I) -> Self
        where
            I: IntoIterator<Item = (K, V)>,
        {
            let mut table = InlineTable { items: to_key_value_pairs(iter), ..Default::default() };
            decorate_inline_table(&mut table);
            Value::InlineTable(table)
        }
    }
}
pub(crate) mod index {
    use super::{
        document::Document,
        formatted::to_table_key_value,
        table::{value, Item, Table},
        value::{InlineTable, Value},
    };
    use std::ops;
    pub(crate) mod private {
        pub trait Sealed {}
        impl Sealed for usize {}
        impl Sealed for str {}
        impl Sealed for String {}
        impl<'a, T: ?Sized> Sealed for &'a T where T: Sealed {}
    }
    pub trait Index: private::Sealed {
        /// Return `Option::None` if the key is not already in the array or table.
        #[doc(hidden)]
        fn index<'v>(&self, v: &'v Item) -> Option<&'v Item>;
        /// Panic if array index out of bounds. If key is not already in the table,
        /// insert it with a value of `Item::None`. Panic if `v` has a type that cannot be
        /// indexed into, except if `v` is `Item::None` then it can be treated as an empty
        /// inline table.
        #[doc(hidden)]
        fn index_or_insert<'v>(&self, v: &'v mut Item) -> &'v mut Item;
    }
    impl Index for usize {
        fn index<'v>(&self, v: &'v Item) -> Option<&'v Item> {
            match *v {
                Item::ArrayOfTables(ref aot) => aot.values.get(*self),
                Item::Value(ref a) if a.is_array() => a.as_array().unwrap().values.get(*self),
                _ => None,
            }
        }
        fn index_or_insert<'v>(&self, v: &'v mut Item) -> &'v mut Item {
            match *v {
                Item::ArrayOfTables(ref mut vec) => {
                    vec.values.get_mut(*self).expect("index out of bounds")
                }
                Item::Value(ref mut a) if a.is_array() => {
                    a.as_array_mut().unwrap().values.get_mut(*self).expect("index out of bounds")
                }
                _ => panic!("cannot access index {}", self),
            }
        }
    }
    impl Index for str {
        fn index<'v>(&self, v: &'v Item) -> Option<&'v Item> {
            match *v {
                Item::Table(ref t) => t.get(self),
                Item::Value(ref v) if v.is_inline_table() => {
                    v.as_inline_table().and_then(|t| t.items.get(self).map(|kv| &kv.value))
                }
                _ => None,
            }
        }
        fn index_or_insert<'v>(&self, v: &'v mut Item) -> &'v mut Item {
            if let Item::None = *v {
                let mut t = InlineTable::default();
                t.items.insert(self.to_owned(), to_table_key_value(self, Item::None));
                *v = value(Value::InlineTable(t));
            }
            match *v {
                Item::Table(ref mut t) => t.entry(self).or_insert(Item::None),
                Item::Value(ref mut v) if v.is_inline_table() => {
                    &mut v
                        .as_inline_table_mut()
                        .unwrap()
                        .items
                        .entry(self.to_owned())
                        .or_insert(to_table_key_value(self, Item::None))
                        .value
                }
                _ => panic!("cannot access key {}", self),
            }
        }
    }
    impl Index for String {
        fn index<'v>(&self, v: &'v Item) -> Option<&'v Item> {
            self[..].index(v)
        }
        fn index_or_insert<'v>(&self, v: &'v mut Item) -> &'v mut Item {
            self[..].index_or_insert(v)
        }
    }
    impl<'a, T: ?Sized> Index for &'a T
    where
        T: Index,
    {
        fn index<'v>(&self, v: &'v Item) -> Option<&'v Item> {
            (**self).index(v)
        }
        fn index_or_insert<'v>(&self, v: &'v mut Item) -> &'v mut Item {
            (**self).index_or_insert(v)
        }
    }
    impl<I> ops::Index<I> for Item
    where
        I: Index,
    {
        type Output = Item;
        fn index(&self, index: I) -> &Item {
            static NONE: Item = Item::None;
            index.index(self).unwrap_or(&NONE)
        }
    }
    impl<I> ops::IndexMut<I> for Item
    where
        I: Index,
    {
        fn index_mut(&mut self, index: I) -> &mut Item {
            index.index_or_insert(self)
        }
    }
    impl<'s> ops::Index<&'s str> for Table {
        type Output = Item;
        fn index(&self, key: &'s str) -> &Item {
            static NONE: Item = Item::None;
            self.get(key).unwrap_or(&NONE)
        }
    }
    impl<'s> ops::IndexMut<&'s str> for Table {
        fn index_mut(&mut self, key: &'s str) -> &mut Item {
            self.entry(key).or_insert(Item::None)
        }
    }
    impl<'s> ops::Index<&'s str> for Document {
        type Output = Item;
        fn index(&self, key: &'s str) -> &Item {
            self.root.index(key)
        }
    }
    impl<'s> ops::IndexMut<&'s str> for Document {
        fn index_mut(&mut self, key: &'s str) -> &mut Item {
            self.root.index_mut(key)
        }
    }
}
pub(crate) mod key {
    use super::parser;
    use combine::stream::state::State;
    use std::str::FromStr;
    /// Key as part of a Key/Value Pair or a table header.
    ///
    /// # Examples
    ///
    /// ```notrust
    /// [dependencies."nom"]
    /// version = "5.0"
    /// 'literal key' = "nonsense"
    /// "basic string key" = 42
    /// ```
    ///
    /// There are 3 types of keys:
    ///
    /// 1. Bare keys (`version` and `dependencies`)
    ///
    /// 2. Basic quoted keys (`"basic string key"` and `"nom"`)
    ///
    /// 3. Literal quoted keys (`'literal key'`)
    ///
    /// For details see [toml spec](https://github.com/toml-lang/toml/#keyvalue-pair).
    ///
    /// To parse a key use `FromStr` trait implementation: `"string".parse::<Key>()`.
    #[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Clone)]
    pub struct Key {
        key: String,
        raw: String,
    }
    impl FromStr for Key {
        type Err = parser::TomlError;
        /// Tries to parse a key from a &str,
        /// if fails, tries as basic quoted key (surrounds with "")
        /// and then literal quoted key (surrounds with '')
        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let basic = format!("\"{}\"", s);
            let literal = format!("'{}'", s);
            Key::try_parse(s)
                .or_else(|_| Key::try_parse(&basic))
                .or_else(|_| Key::try_parse(&literal))
        }
    }
    impl Key {
        fn try_parse(s: &str) -> Result<Key, parser::TomlError> {
            use combine::Parser;
            let result = parser::key_parser().easy_parse(State::new(s));
            match result {
                Ok((_, ref rest)) if !rest.input.is_empty() => {
                    Err(parser::TomlError::from_unparsed(rest.positioner, s))
                }
                Ok(((raw, key), _)) => Ok(Key::new(raw, key)),
                Err(e) => Err(parser::TomlError::new(e, s)),
            }
        }
        pub(crate) fn new(raw: &str, key: String) -> Self {
            Self { raw: raw.into(), key }
        }
        /// Returns the parsed key value.
        pub fn get(&self) -> &str {
            &self.key
        }
        /// Returns the key raw representation.
        pub fn raw(&self) -> &str {
            &self.raw
        }
    }
    #[doc(hidden)]
    impl Into<String> for Key {
        fn into(self) -> String {
            self.key
        }
    }
}
pub(crate) mod parser {
    #![allow(clippy::unneeded_field_pattern)]
    #![allow(clippy::toplevel_ref_arg)]
    pub(crate) mod array {
        use super::super::{
            formatted::decorated,
            parser::{errors::CustomError, trivia::ws_comment_newline, value::value},
            value::{Array, Value},
        };
        use combine::{char::char, range::recognize_with_value, stream::RangeStream, *};
        #[allow(non_camel_case_types)]
        pub struct array<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> Array>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for array<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = Array;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Array, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let array { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            between(
                                char(ARRAY_OPEN),
                                char(ARRAY_CLOSE),
                                array_values().and_then(|(v, c, t)| array_from_vec(v, c, t)),
                            )
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let array { .. } = *self;
                let mut parser = {
                    {
                        between(
                            char(ARRAY_OPEN),
                            char(ARRAY_CLOSE),
                            array_values().and_then(|(v, c, t)| array_from_vec(v, c, t)),
                        )
                    }
                };
                {
                    let _: &mut dyn (::combine::Parser<Input = I, Output = Array, PartialState = _>) =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let array { .. } = *self;
                let mut parser = {
                    {
                        between(
                            char(ARRAY_OPEN),
                            char(ARRAY_CLOSE),
                            array_values().and_then(|(v, c, t)| array_from_vec(v, c, t)),
                        )
                    }
                };
                {
                    let _: &mut dyn (::combine::Parser<Input = I, Output = Array, PartialState = _>) =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn array<'a, I>() -> array<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            array { __marker: ::combine::lib::marker::PhantomData }
        }
        fn array_from_vec(
            v: Vec<Value>,
            comma: bool,
            trailing: &str,
        ) -> Result<Array, CustomError> {
            let mut array = Array::default();
            array.trailing_comma = comma;
            array.trailing = String::from(trailing);
            for val in v {
                let err = Err(CustomError::MixedArrayType {
                    got: format!("{:?}", val.get_type()),
                    expected: format!("{:?}", array.value_type()),
                });
                if !array.push_value(val, false) {
                    return err;
                }
            }
            Ok(array)
        }
        const ARRAY_OPEN: char = '[';
        const ARRAY_CLOSE: char = ']';
        const ARRAY_SEP: char = ',';
        #[allow(non_camel_case_types)]
        pub struct array_values<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> (Vec<Value>, bool, &'a str)>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for array_values<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = (Vec<Value>, bool, &'a str);
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<(Vec<Value>, bool, &'a str), I>
            where
                M: ::combine::parser::ParseMode,
            {
                let array_values { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            (
                                optional(
                                    recognize_with_value(sep_end_by1(
                                        array_value(),
                                        char(ARRAY_SEP),
                                    ))
                                    .map(|(r, v): (&'a str, _)| (v, r.ends_with(','))),
                                ),
                                ws_comment_newline(),
                            )
                                .map(|(v, t)| {
                                    let (v, c) = v.unwrap_or_default();
                                    (v, c, t)
                                })
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let array_values { .. } = *self;
                let mut parser = {
                    {
                        (
                            optional(
                                recognize_with_value(sep_end_by1(array_value(), char(ARRAY_SEP)))
                                    .map(|(r, v): (&'a str, _)| (v, r.ends_with(','))),
                            ),
                            ws_comment_newline(),
                        )
                            .map(|(v, t)| {
                                let (v, c) = v.unwrap_or_default();
                                (v, c, t)
                            })
                    }
                };
                {
                    let _: &mut dyn (::combine::Parser<
                        Input = I,
                        Output = (Vec<Value>, bool, &'a str),
                        PartialState = _,
                    >) = &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let array_values { .. } = *self;
                let mut parser = {
                    {
                        (
                            optional(
                                recognize_with_value(sep_end_by1(array_value(), char(ARRAY_SEP)))
                                    .map(|(r, v): (&'a str, _)| (v, r.ends_with(','))),
                            ),
                            ws_comment_newline(),
                        )
                            .map(|(v, t)| {
                                let (v, c) = v.unwrap_or_default();
                                (v, c, t)
                            })
                    }
                };
                {
                    let _: &mut ::combine::Parser<
                        Input = I,
                        Output = (Vec<Value>, bool, &'a str),
                        PartialState = _,
                    > = &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn array_values<'a, I>() -> array_values<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            array_values { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct array_value<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> Value>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for array_value<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = Value;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Value, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let array_value { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            attempt((ws_comment_newline(), value(), ws_comment_newline()))
                                .map(|(ws1, v, ws2)| decorated(v, ws1, ws2))
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let array_value { .. } = *self;
                let mut parser = {
                    {
                        attempt((ws_comment_newline(), value(), ws_comment_newline()))
                            .map(|(ws1, v, ws2)| decorated(v, ws1, ws2))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = Value, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let array_value { .. } = *self;
                let mut parser = {
                    {
                        attempt((ws_comment_newline(), value(), ws_comment_newline()))
                            .map(|(ws1, v, ws2)| decorated(v, ws1, ws2))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = Value, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn array_value<'a, I>() -> array_value<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            array_value { __marker: ::combine::lib::marker::PhantomData }
        }
    }
    pub(crate) mod datetime {
        use super::super::value;
        use combine::{
            char::{char, digit},
            combinator::{skip_count_min_max, SkipCountMinMax},
            range::{recognize, recognize_with_value},
            stream::RangeStream,
            *,
        };
        #[inline]
        pub fn repeat<P: Parser>(count: usize, parser: P) -> SkipCountMinMax<P> {
            skip_count_min_max(count, count, parser)
        }
        #[allow(non_camel_case_types)]
        pub struct date_time<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> value::DateTime>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for date_time<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = value::DateTime;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<value::DateTime, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let date_time { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            recognize_with_value((
                                full_date(),
                                optional((char('T'), partial_time(), optional(time_offset()))),
                            ))
                            .and_then(|(s, (_, opt))| match opt {
                                Some((_, _, Some(_))) => chrono::DateTime::parse_from_rfc3339(s)
                                    .map(value::DateTime::OffsetDateTime),
                                Some(_) => s
                                    .parse::<chrono::NaiveDateTime>()
                                    .map(value::DateTime::LocalDateTime),
                                None => {
                                    s.parse::<chrono::NaiveDate>().map(value::DateTime::LocalDate)
                                }
                            })
                            .or(recognize(partial_time())
                                .and_then(str::parse)
                                .message("While parsing a Time")
                                .map(value::DateTime::LocalTime))
                            .message("While parsing a Date-Time")
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let date_time { .. } = *self;
                let mut parser = {
                    {
                        recognize_with_value((
                            full_date(),
                            optional((char('T'), partial_time(), optional(time_offset()))),
                        ))
                        .and_then(|(s, (_, opt))| match opt {
                            Some((_, _, Some(_))) => chrono::DateTime::parse_from_rfc3339(s)
                                .map(value::DateTime::OffsetDateTime),
                            Some(_) => s
                                .parse::<chrono::NaiveDateTime>()
                                .map(value::DateTime::LocalDateTime),
                            None => s.parse::<chrono::NaiveDate>().map(value::DateTime::LocalDate),
                        })
                        .or(recognize(partial_time())
                            .and_then(str::parse)
                            .message("While parsing a Time")
                            .map(value::DateTime::LocalTime))
                        .message("While parsing a Date-Time")
                    }
                };
                {
                    let _: &mut dyn (::combine::Parser<
                        Input = I,
                        Output = value::DateTime,
                        PartialState = _,
                    >) = &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let date_time { .. } = *self;
                let mut parser = {
                    {
                        recognize_with_value((
                            full_date(),
                            optional((char('T'), partial_time(), optional(time_offset()))),
                        ))
                        .and_then(|(s, (_, opt))| match opt {
                            Some((_, _, Some(_))) => chrono::DateTime::parse_from_rfc3339(s)
                                .map(value::DateTime::OffsetDateTime),
                            Some(_) => s
                                .parse::<chrono::NaiveDateTime>()
                                .map(value::DateTime::LocalDateTime),
                            None => s.parse::<chrono::NaiveDate>().map(value::DateTime::LocalDate),
                        })
                        .or(recognize(partial_time())
                            .and_then(str::parse)
                            .message("While parsing a Time")
                            .map(value::DateTime::LocalTime))
                        .message("While parsing a Date-Time")
                    }
                };
                {
                    let _: &mut ::combine::Parser<
                        Input = I,
                        Output = value::DateTime,
                        PartialState = _,
                    > = &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn date_time<'a, I>() -> date_time<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            date_time { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct full_date<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> &'a str>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for full_date<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = &'a str;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<&'a str, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let full_date { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            recognize((
                                attempt((repeat(4, digit()), char('-'))),
                                repeat(2, digit()),
                                char('-'),
                                repeat(2, digit()),
                            ))
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let full_date { .. } = *self;
                let mut parser = {
                    {
                        recognize((
                            attempt((repeat(4, digit()), char('-'))),
                            repeat(2, digit()),
                            char('-'),
                            repeat(2, digit()),
                        ))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let full_date { .. } = *self;
                let mut parser = {
                    {
                        recognize((
                            attempt((repeat(4, digit()), char('-'))),
                            repeat(2, digit()),
                            char('-'),
                            repeat(2, digit()),
                        ))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn full_date<'a, I>() -> full_date<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            full_date { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct partial_time<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> ()>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for partial_time<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = ();
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<(), I>
            where
                M: ::combine::parser::ParseMode,
            {
                let partial_time { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            (
                                attempt((repeat(2, digit()), char(':'))),
                                repeat(2, digit()),
                                char(':'),
                                repeat(2, digit()),
                                optional(attempt(char('.')).and(skip_many1(digit()))),
                            )
                                .map(|_| ())
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let partial_time { .. } = *self;
                let mut parser = {
                    {
                        (
                            attempt((repeat(2, digit()), char(':'))),
                            repeat(2, digit()),
                            char(':'),
                            repeat(2, digit()),
                            optional(attempt(char('.')).and(skip_many1(digit()))),
                        )
                            .map(|_| ())
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = (), PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let partial_time { .. } = *self;
                let mut parser = {
                    {
                        (
                            attempt((repeat(2, digit()), char(':'))),
                            repeat(2, digit()),
                            char(':'),
                            repeat(2, digit()),
                            optional(attempt(char('.')).and(skip_many1(digit()))),
                        )
                            .map(|_| ())
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = (), PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn partial_time<'a, I>() -> partial_time<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            partial_time { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct time_offset<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> ()>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for time_offset<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = ();
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<(), I>
            where
                M: ::combine::parser::ParseMode,
            {
                let time_offset { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            attempt(char('Z'))
                                .map(|_| ())
                                .or((
                                    attempt(choice([char('+'), char('-')])),
                                    repeat(2, digit()),
                                    char(':'),
                                    repeat(2, digit()),
                                )
                                    .map(|_| ()))
                                .message("While parsing a Time Offset")
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let time_offset { .. } = *self;
                let mut parser = {
                    {
                        attempt(char('Z'))
                            .map(|_| ())
                            .or((
                                attempt(choice([char('+'), char('-')])),
                                repeat(2, digit()),
                                char(':'),
                                repeat(2, digit()),
                            )
                                .map(|_| ()))
                            .message("While parsing a Time Offset")
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = (), PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let time_offset { .. } = *self;
                let mut parser = {
                    {
                        attempt(char('Z'))
                            .map(|_| ())
                            .or((
                                attempt(choice([char('+'), char('-')])),
                                repeat(2, digit()),
                                char(':'),
                                repeat(2, digit()),
                            )
                                .map(|_| ()))
                            .message("While parsing a Time Offset")
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = (), PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn time_offset<'a, I>() -> time_offset<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            time_offset { __marker: ::combine::lib::marker::PhantomData }
        }
    }
    pub(crate) mod document {
        use super::super::{
            decor::Repr,
            document::Document,
            formatted::decorated,
            parser::{
                errors::CustomError,
                inline_table::KEYVAL_SEP,
                key::key,
                table::table,
                trivia::{comment, line_ending, line_trailing, newline, ws},
                value::value,
                TomlError, TomlParser,
            },
            table::{Item, TableKeyValue},
        };
        use combine::{
            char::char,
            range::recognize,
            stream::{state::State, RangeStream},
            Parser, *,
        };
        use std::{cell::RefCell, mem, ops::DerefMut};
        #[allow(non_camel_case_types)]
        struct parse_comment<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            pub parser: &'b RefCell<TomlParser>,
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> ()>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, 'b, I> ::combine::Parser for parse_comment<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = ();
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<(), I>
            where
                M: ::combine::parser::ParseMode,
            {
                let parse_comment { parser: ref mut parser, .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            (comment(), line_ending())
                                .map(|(c, e)| parser.borrow_mut().deref_mut().on_comment(c, e))
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let parse_comment { parser: ref mut parser, .. } = *self;
                let mut parser = {
                    {
                        (comment(), line_ending())
                            .map(|(c, e)| parser.borrow_mut().deref_mut().on_comment(c, e))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = (), PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let parse_comment { parser: ref mut parser, .. } = *self;
                let mut parser = {
                    {
                        (comment(), line_ending())
                            .map(|(c, e)| parser.borrow_mut().deref_mut().on_comment(c, e))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = (), PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        fn parse_comment<'a, 'b, I>(parser: &'b RefCell<TomlParser>) -> parse_comment<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            parse_comment { parser: parser, __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        struct parse_ws<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            pub parser: &'b RefCell<TomlParser>,
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> ()>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, 'b, I> ::combine::Parser for parse_ws<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = ();
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<(), I>
            where
                M: ::combine::parser::ParseMode,
            {
                let parse_ws { parser: ref mut parser, .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        { ws().map(|w| parser.borrow_mut().deref_mut().on_ws(w)) }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let parse_ws { parser: ref mut parser, .. } = *self;
                let mut parser = {
                    { ws().map(|w| parser.borrow_mut().deref_mut().on_ws(w)) }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = (), PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let parse_ws { parser: ref mut parser, .. } = *self;
                let mut parser = {
                    { ws().map(|w| parser.borrow_mut().deref_mut().on_ws(w)) }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = (), PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        fn parse_ws<'a, 'b, I>(parser: &'b RefCell<TomlParser>) -> parse_ws<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            parse_ws { parser: parser, __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        struct parse_newline<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            pub parser: &'b RefCell<TomlParser>,
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> ()>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, 'b, I> ::combine::Parser for parse_newline<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = ();
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<(), I>
            where
                M: ::combine::parser::ParseMode,
            {
                let parse_newline { parser: ref mut parser, .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        { recognize(newline()).map(|w| parser.borrow_mut().deref_mut().on_ws(w)) }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let parse_newline { parser: ref mut parser, .. } = *self;
                let mut parser = {
                    { recognize(newline()).map(|w| parser.borrow_mut().deref_mut().on_ws(w)) }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = (), PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let parse_newline { parser: ref mut parser, .. } = *self;
                let mut parser = {
                    { recognize(newline()).map(|w| parser.borrow_mut().deref_mut().on_ws(w)) }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = (), PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        fn parse_newline<'a, 'b, I>(parser: &'b RefCell<TomlParser>) -> parse_newline<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            parse_newline { parser: parser, __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        struct keyval<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            pub parser: &'b RefCell<TomlParser>,
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> ()>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, 'b, I> ::combine::Parser for keyval<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = ();
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<(), I>
            where
                M: ::combine::parser::ParseMode,
            {
                let keyval { parser: ref mut parser, .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            parse_keyval().and_then(|(k, kv)| {
                                parser.borrow_mut().deref_mut().on_keyval(k, kv)
                            })
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let keyval { parser: ref mut parser, .. } = *self;
                let mut parser = {
                    {
                        parse_keyval()
                            .and_then(|(k, kv)| parser.borrow_mut().deref_mut().on_keyval(k, kv))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = (), PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let keyval { parser: ref mut parser, .. } = *self;
                let mut parser = {
                    {
                        parse_keyval()
                            .and_then(|(k, kv)| parser.borrow_mut().deref_mut().on_keyval(k, kv))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = (), PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        fn keyval<'a, 'b, I>(parser: &'b RefCell<TomlParser>) -> keyval<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            keyval { parser: parser, __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        struct parse_keyval<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> (String, TableKeyValue)>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for parse_keyval<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = (String, TableKeyValue);
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<(String, TableKeyValue), I>
            where
                M: ::combine::parser::ParseMode,
            {
                let parse_keyval { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        ((key(), ws()), char(KEYVAL_SEP), (ws(), value(), line_trailing())).map(
                            |(k, _, v)| {
                                let (pre, v, suf) = v;
                                let v = decorated(v, pre, suf);
                                let ((raw, key), suf) = k;
                                (
                                    key,
                                    TableKeyValue {
                                        key: Repr::new("", raw, suf),
                                        value: Item::Value(v),
                                    },
                                )
                            },
                        )
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let parse_keyval { .. } = *self;
                let mut parser = {
                    ((key(), ws()), char(KEYVAL_SEP), (ws(), value(), line_trailing())).map(
                        |(k, _, v)| {
                            let (pre, v, suf) = v;
                            let v = decorated(v, pre, suf);
                            let ((raw, key), suf) = k;
                            (
                                key,
                                TableKeyValue {
                                    key: Repr::new("", raw, suf),
                                    value: Item::Value(v),
                                },
                            )
                        },
                    )
                };
                {
                    let _: &mut ::combine::Parser<
                        Input = I,
                        Output = (String, TableKeyValue),
                        PartialState = _,
                    > = &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let parse_keyval { .. } = *self;
                let mut parser = {
                    ((key(), ws()), char(KEYVAL_SEP), (ws(), value(), line_trailing())).map(
                        |(k, _, v)| {
                            let (pre, v, suf) = v;
                            let v = decorated(v, pre, suf);
                            let ((raw, key), suf) = k;
                            (
                                key,
                                TableKeyValue {
                                    key: Repr::new("", raw, suf),
                                    value: Item::Value(v),
                                },
                            )
                        },
                    )
                };
                {
                    let _: &mut ::combine::Parser<
                        Input = I,
                        Output = (String, TableKeyValue),
                        PartialState = _,
                    > = &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        fn parse_keyval<'a, I>() -> parse_keyval<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            parse_keyval { __marker: ::combine::lib::marker::PhantomData }
        }
        impl TomlParser {
            pub fn parse(s: &str) -> Result<Document, TomlError> {
                let parser = RefCell::new(Self::default());
                let input = State::new(s);
                let parsed = parse_ws(&parser)
                    .with(choice((
                        eof(),
                        skip_many1(
                            choice((
                                parse_comment(&parser),
                                keyval(&parser),
                                table(&parser),
                                parse_newline(&parser),
                            ))
                            .skip(parse_ws(&parser)),
                        ),
                    )))
                    .easy_parse(input);
                match parsed {
                    Ok((_, ref rest)) if !rest.input.is_empty() => {
                        Err(TomlError::from_unparsed(rest.positioner, s))
                    }
                    Ok(..) => Ok(*parser.into_inner().document),
                    Err(e) => Err(TomlError::new(e, s)),
                }
            }
            fn on_ws(&mut self, w: &str) {
                self.document.trailing.push_str(w);
            }
            fn on_comment(&mut self, c: &str, e: &str) {
                self.document.trailing.push_str(c);
                self.document.trailing.push_str(e);
            }
            fn on_keyval(&mut self, key: String, mut kv: TableKeyValue) -> Result<(), CustomError> {
                let prefix = mem::replace(&mut self.document.trailing, String::new());
                kv.key.decor.prefix = prefix + &kv.key.decor.prefix;
                let root = self.document.as_table_mut();
                let table = Self::descend_path(root, self.current_table_path.as_slice(), 0)
                    .expect("the table path is valid; qed");
                if table.contains_key(&key) {
                    Err(CustomError::DuplicateKey { key, table: "<unknown>".into() })
                } else {
                    let tkv = TableKeyValue { key: kv.key, value: kv.value };
                    table.items.insert(key, tkv);
                    Ok(())
                }
            }
        }
    }
    pub(crate) mod errors {
        use combine::{
            easy::Errors as ParseError,
            stream::{easy::Error, state::SourcePosition},
        };
        use std::fmt::{Display, Formatter, Result};
        /// Type representing a TOML parse error
        #[derive(Debug, Clone, Eq, PartialEq, Hash)]
        pub struct TomlError {
            message: String,
        }
        impl TomlError {
            pub(crate) fn new(error: ParseError<char, &str, SourcePosition>, input: &str) -> Self {
                Self { message: format!("{}", FancyError::new(error, input)) }
            }
            pub(crate) fn from_unparsed(pos: SourcePosition, input: &str) -> Self {
                Self::new(ParseError::new(pos, CustomError::UnparsedLine.into()), input)
            }
        }
        /// Displays a TOML parse error
        ///
        /// # Example
        ///
        /// TOML parse error at line 1, column 10
        ///   |
        /// 1 | 00:32:00.a999999
        ///   |          ^
        /// Unexpected `a`
        /// Expected `digit`
        /// While parsing a Time
        /// While parsing a Date-Time
        impl Display for TomlError {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result {
                write!(f, "{}", self.message)
            }
        }
        impl std::error::Error for TomlError {}
        #[derive(Debug)]
        pub(crate) struct FancyError<'a> {
            error: ParseError<char, &'a str, SourcePosition>,
            input: &'a str,
        }
        impl<'a> FancyError<'a> {
            pub(crate) fn new(
                error: ParseError<char, &'a str, SourcePosition>,
                input: &'a str,
            ) -> Self {
                Self { error, input }
            }
        }
        impl<'a> Display for FancyError<'a> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result {
                let SourcePosition { line, column } = self.error.position;

                let offset = line.to_string().len();
                let content = self.input.split('\n').nth((line - 1) as usize).expect("line");

                writeln!(f, "TOML parse error at line {}, column {}", line, column)?;

                //   |
                for _ in 0..=offset {
                    write!(f, " ")?;
                }
                writeln!(f, "|")?;

                // 1 | 00:32:00.a999999
                write!(f, "{} | ", line)?;
                writeln!(f, "{}", content)?;

                //   |          ^
                for _ in 0..=offset {
                    write!(f, " ")?;
                }
                write!(f, "|")?;
                for _ in 0..column {
                    write!(f, " ")?;
                }
                writeln!(f, "^")?;

                Error::fmt_errors(self.error.errors.as_ref(), f)
            }
        }
        #[derive(Debug, Clone)]
        pub enum CustomError {
            MixedArrayType { got: String, expected: String },
            DuplicateKey { key: String, table: String },
            InvalidHexEscape(u32),
            UnparsedLine,
        }
        impl std::error::Error for CustomError {}
        impl Display for CustomError {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result {
                match *self {
                    CustomError::MixedArrayType { ref got, ref expected } => {
                        writeln!(f, "Mixed types in array: {} and {}", expected, got)
                    }
                    CustomError::DuplicateKey { ref key, ref table } => {
                        writeln!(f, "Duplicate key `{}` in `{}` table", key, table)
                    }
                    CustomError::InvalidHexEscape(ref h) => {
                        writeln!(f, "Invalid hex escape code: {:x} ", h)
                    }
                    CustomError::UnparsedLine => writeln!(f, "Could not parse the line"),
                }
            }
        }
    }
    pub(crate) mod inline_table {
        use super::super::{
            decor::Repr,
            formatted::decorated,
            parser::{errors::CustomError, key::key, trivia::ws, value::value},
            table::{Item, TableKeyValue},
            value::InlineTable,
        };
        use combine::{char::char, stream::RangeStream, *};
        #[allow(non_camel_case_types)]
        pub struct inline_table<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> InlineTable>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for inline_table<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = InlineTable;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<InlineTable, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let inline_table { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            between(
                                char(INLINE_TABLE_OPEN),
                                char(INLINE_TABLE_CLOSE),
                                inline_table_keyvals().and_then(|(p, v)| table_from_pairs(p, v)),
                            )
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let inline_table { .. } = *self;
                let mut parser = {
                    {
                        between(
                            char(INLINE_TABLE_OPEN),
                            char(INLINE_TABLE_CLOSE),
                            inline_table_keyvals().and_then(|(p, v)| table_from_pairs(p, v)),
                        )
                    }
                };
                {
                    let _: &mut ::combine::Parser<
                        Input = I,
                        Output = InlineTable,
                        PartialState = _,
                    > = &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let inline_table { .. } = *self;
                let mut parser = {
                    {
                        between(
                            char(INLINE_TABLE_OPEN),
                            char(INLINE_TABLE_CLOSE),
                            inline_table_keyvals().and_then(|(p, v)| table_from_pairs(p, v)),
                        )
                    }
                };
                {
                    let _: &mut ::combine::Parser<
                        Input = I,
                        Output = InlineTable,
                        PartialState = _,
                    > = &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn inline_table<'a, I>() -> inline_table<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            inline_table { __marker: ::combine::lib::marker::PhantomData }
        }
        fn table_from_pairs(
            preamble: &str,
            v: Vec<(String, TableKeyValue)>,
        ) -> Result<InlineTable, CustomError> {
            let mut table = InlineTable::default();
            table.preamble = String::from(preamble);
            for (k, kv) in v {
                if table.contains_key(&k) {
                    return Err(CustomError::DuplicateKey { key: k, table: "inline".into() });
                }
                table.items.insert(k, kv);
            }
            Ok(table)
        }
        const INLINE_TABLE_OPEN: char = '{';
        const INLINE_TABLE_CLOSE: char = '}';
        const INLINE_TABLE_SEP: char = ',';
        pub(crate) const KEYVAL_SEP: char = '=';
        #[allow(non_camel_case_types)]
        pub struct inline_table_keyvals<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<
                fn(I) -> (&'a str, Vec<(String, TableKeyValue)>),
            >,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for inline_table_keyvals<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = (&'a str, Vec<(String, TableKeyValue)>);
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<(&'a str, Vec<(String, TableKeyValue)>), I>
            where
                M: ::combine::parser::ParseMode,
            {
                let inline_table_keyvals { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        { (sep_by(keyval(), char(INLINE_TABLE_SEP)), ws()).map(|(v, w)| (w, v)) }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let inline_table_keyvals { .. } = *self;
                let mut parser = {
                    { (sep_by(keyval(), char(INLINE_TABLE_SEP)), ws()).map(|(v, w)| (w, v)) }
                };
                {
                    let _: &mut ::combine::Parser<
                        Input = I,
                        Output = (&'a str, Vec<(String, TableKeyValue)>),
                        PartialState = _,
                    > = &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let inline_table_keyvals { .. } = *self;
                let mut parser = {
                    { (sep_by(keyval(), char(INLINE_TABLE_SEP)), ws()).map(|(v, w)| (w, v)) }
                };
                {
                    let _: &mut ::combine::Parser<
                        Input = I,
                        Output = (&'a str, Vec<(String, TableKeyValue)>),
                        PartialState = _,
                    > = &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn inline_table_keyvals<'a, I>() -> inline_table_keyvals<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            inline_table_keyvals { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct keyval<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> (String, TableKeyValue)>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for keyval<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = (String, TableKeyValue);
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<(String, TableKeyValue), I>
            where
                M: ::combine::parser::ParseMode,
            {
                let keyval { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            (attempt((ws(), key(), ws())), char(KEYVAL_SEP), (ws(), value(), ws()))
                                .map(|(k, _, v)| {
                                    let (pre, v, suf) = v;
                                    let v = decorated(v, pre, suf);
                                    let (pre, (raw, key), suf) = k;
                                    (
                                        key,
                                        TableKeyValue {
                                            key: Repr::new(pre, raw, suf),
                                            value: Item::Value(v),
                                        },
                                    )
                                })
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let keyval { .. } = *self;
                let mut parser = {
                    {
                        (attempt((ws(), key(), ws())), char(KEYVAL_SEP), (ws(), value(), ws())).map(
                            |(k, _, v)| {
                                let (pre, v, suf) = v;
                                let v = decorated(v, pre, suf);
                                let (pre, (raw, key), suf) = k;
                                (
                                    key,
                                    TableKeyValue {
                                        key: Repr::new(pre, raw, suf),
                                        value: Item::Value(v),
                                    },
                                )
                            },
                        )
                    }
                };
                {
                    let _: &mut ::combine::Parser<
                        Input = I,
                        Output = (String, TableKeyValue),
                        PartialState = _,
                    > = &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let keyval { .. } = *self;
                let mut parser = {
                    {
                        (attempt((ws(), key(), ws())), char(KEYVAL_SEP), (ws(), value(), ws())).map(
                            |(k, _, v)| {
                                let (pre, v, suf) = v;
                                let v = decorated(v, pre, suf);
                                let (pre, (raw, key), suf) = k;
                                (
                                    key,
                                    TableKeyValue {
                                        key: Repr::new(pre, raw, suf),
                                        value: Item::Value(v),
                                    },
                                )
                            },
                        )
                    }
                };
                {
                    let _: &mut ::combine::Parser<
                        Input = I,
                        Output = (String, TableKeyValue),
                        PartialState = _,
                    > = &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn keyval<'a, I>() -> keyval<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            keyval { __marker: ::combine::lib::marker::PhantomData }
        }
    }
    pub(crate) mod key {
        use super::super::parser::strings::{basic_string, literal_string};
        use combine::{
            range::{recognize_with_value, take_while1},
            stream::RangeStream,
            *,
        };
        #[inline]
        fn is_unquoted_char(c: char) -> bool {
            match c {
                'A'..='Z' | 'a'..='z' | '0'..='9' | '-' | '_' => true,
                _ => false,
            }
        }
        #[allow(non_camel_case_types)]
        pub struct unquoted_key<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> &'a str>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for unquoted_key<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = &'a str;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<&'a str, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let unquoted_key { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        { take_while1(is_unquoted_char) }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let unquoted_key { .. } = *self;
                let mut parser = {
                    { take_while1(is_unquoted_char) }
                };
                {
                    let _: &mut dyn (::combine::Parser<
                        Input = I,
                        Output = &'a str,
                        PartialState = _,
                    >) = &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let unquoted_key { .. } = *self;
                let mut parser = {
                    { take_while1(is_unquoted_char) }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn unquoted_key<'a, I>() -> unquoted_key<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            unquoted_key { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct key<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> (&'a str, String)>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for key<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = (&'a str, String);
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<(&'a str, String), I>
            where
                M: ::combine::parser::ParseMode,
            {
                let key { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            recognize_with_value(choice((
                                basic_string(),
                                literal_string().map(|s: &'a str| s.into()),
                                unquoted_key().map(|s: &'a str| s.into()),
                            )))
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let key { .. } = *self;
                let mut parser = {
                    {
                        recognize_with_value(choice((
                            basic_string(),
                            literal_string().map(|s: &'a str| s.into()),
                            unquoted_key().map(|s: &'a str| s.into()),
                        )))
                    }
                };
                {
                    let _: &mut ::combine::Parser<
                        Input = I,
                        Output = (&'a str, String),
                        PartialState = _,
                    > = &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let key { .. } = *self;
                let mut parser = {
                    {
                        recognize_with_value(choice((
                            basic_string(),
                            literal_string().map(|s: &'a str| s.into()),
                            unquoted_key().map(|s: &'a str| s.into()),
                        )))
                    }
                };
                {
                    let _: &mut ::combine::Parser<
                        Input = I,
                        Output = (&'a str, String),
                        PartialState = _,
                    > = &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn key<'a, I>() -> key<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            key { __marker: ::combine::lib::marker::PhantomData }
        }
    }
    pub(crate) mod numbers {
        use combine::{
            char::{char, digit, hex_digit, oct_digit, string},
            range::{range, recognize},
            stream::RangeStream,
            *,
        };
        #[allow(non_camel_case_types)]
        pub struct boolean<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> bool>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for boolean<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = bool;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<bool, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let boolean { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            choice(((char('t'), range("rue")), (char('f'), range("alse"))))
                                .map(|p| p.0 == 't')
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let boolean { .. } = *self;
                let mut parser = {
                    {
                        choice(((char('t'), range("rue")), (char('f'), range("alse"))))
                            .map(|p| p.0 == 't')
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = bool, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let boolean { .. } = *self;
                let mut parser = {
                    {
                        choice(((char('t'), range("rue")), (char('f'), range("alse"))))
                            .map(|p| p.0 == 't')
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = bool, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn boolean<'a, I>() -> boolean<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            boolean { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct parse_integer<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> &'a str>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for parse_integer<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = &'a str;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<&'a str, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let parse_integer { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            recognize((
                                optional(choice([char('-'), char('+')])),
                                choice((
                                    char('0'),
                                    (
                                        satisfy(|c| '1' <= c && c <= '9'),
                                        skip_many((optional(char('_')), skip_many1(digit()))),
                                    )
                                        .map(|t| t.0),
                                )),
                            ))
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let parse_integer { .. } = *self;
                let mut parser = {
                    {
                        recognize((
                            optional(choice([char('-'), char('+')])),
                            choice((
                                char('0'),
                                (
                                    satisfy(|c| '1' <= c && c <= '9'),
                                    skip_many((optional(char('_')), skip_many1(digit()))),
                                )
                                    .map(|t| t.0),
                            )),
                        ))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let parse_integer { .. } = *self;
                let mut parser = {
                    {
                        recognize((
                            optional(choice([char('-'), char('+')])),
                            choice((
                                char('0'),
                                (
                                    satisfy(|c| '1' <= c && c <= '9'),
                                    skip_many((optional(char('_')), skip_many1(digit()))),
                                )
                                    .map(|t| t.0),
                            )),
                        ))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn parse_integer<'a, I>() -> parse_integer<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            parse_integer { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct integer<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> i64>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for integer<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = i64;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<i64, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let integer { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            attempt(parse_hex_integer()).or(attempt(parse_octal_integer()).or(
                                attempt(parse_binary_integer()).or(parse_integer()
                                    .and_then(|s| s.replace("_", "").parse())
                                    .message("While parsing an Integer")),
                            ))
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let integer { .. } = *self;
                let mut parser = {
                    {
                        attempt(parse_hex_integer()).or(attempt(parse_octal_integer()).or(attempt(
                            parse_binary_integer(),
                        )
                        .or(parse_integer()
                            .and_then(|s| s.replace("_", "").parse())
                            .message("While parsing an Integer"))))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = i64, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let integer { .. } = *self;
                let mut parser = {
                    {
                        attempt(parse_hex_integer()).or(attempt(parse_octal_integer()).or(attempt(
                            parse_binary_integer(),
                        )
                        .or(parse_integer()
                            .and_then(|s| s.replace("_", "").parse())
                            .message("While parsing an Integer"))))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = i64, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn integer<'a, I>() -> integer<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            integer { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct parse_hex_integer<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> i64>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for parse_hex_integer<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = i64;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<i64, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let parse_hex_integer { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            string("0x")
                                .with(recognize(
                                    (
                                        hex_digit(),
                                        skip_many((optional(char('_')), skip_many1(hex_digit()))),
                                    )
                                        .map(|t| t.0),
                                ))
                                .and_then(|s: &str| i64::from_str_radix(&s.replace("_", ""), 16))
                                .message("While parsing a hexadecimal Integer")
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let parse_hex_integer { .. } = *self;
                let mut parser = {
                    {
                        string("0x")
                            .with(recognize(
                                (
                                    hex_digit(),
                                    skip_many((optional(char('_')), skip_many1(hex_digit()))),
                                )
                                    .map(|t| t.0),
                            ))
                            .and_then(|s: &str| i64::from_str_radix(&s.replace("_", ""), 16))
                            .message("While parsing a hexadecimal Integer")
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = i64, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let parse_hex_integer { .. } = *self;
                let mut parser = {
                    {
                        string("0x")
                            .with(recognize(
                                (
                                    hex_digit(),
                                    skip_many((optional(char('_')), skip_many1(hex_digit()))),
                                )
                                    .map(|t| t.0),
                            ))
                            .and_then(|s: &str| i64::from_str_radix(&s.replace("_", ""), 16))
                            .message("While parsing a hexadecimal Integer")
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = i64, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn parse_hex_integer<'a, I>() -> parse_hex_integer<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            parse_hex_integer { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct parse_octal_integer<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> i64>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for parse_octal_integer<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = i64;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<i64, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let parse_octal_integer { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            string("0o")
                                .with(recognize(
                                    (
                                        oct_digit(),
                                        skip_many((optional(char('_')), skip_many1(oct_digit()))),
                                    )
                                        .map(|t| t.0),
                                ))
                                .and_then(|s: &str| i64::from_str_radix(&s.replace("_", ""), 8))
                                .message("While parsing an octal Integer")
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let parse_octal_integer { .. } = *self;
                let mut parser = {
                    {
                        string("0o")
                            .with(recognize(
                                (
                                    oct_digit(),
                                    skip_many((optional(char('_')), skip_many1(oct_digit()))),
                                )
                                    .map(|t| t.0),
                            ))
                            .and_then(|s: &str| i64::from_str_radix(&s.replace("_", ""), 8))
                            .message("While parsing an octal Integer")
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = i64, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let parse_octal_integer { .. } = *self;
                let mut parser = {
                    {
                        string("0o")
                            .with(recognize(
                                (
                                    oct_digit(),
                                    skip_many((optional(char('_')), skip_many1(oct_digit()))),
                                )
                                    .map(|t| t.0),
                            ))
                            .and_then(|s: &str| i64::from_str_radix(&s.replace("_", ""), 8))
                            .message("While parsing an octal Integer")
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = i64, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn parse_octal_integer<'a, I>() -> parse_octal_integer<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            parse_octal_integer { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct parse_binary_integer<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> i64>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for parse_binary_integer<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = i64;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<i64, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let parse_binary_integer { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            string("0b")
                                .with(recognize(
                                    (
                                        satisfy(|c: char| c.is_digit(0x2)),
                                        skip_many((
                                            optional(char('_')),
                                            skip_many1(satisfy(|c: char| c.is_digit(0x2))),
                                        )),
                                    )
                                        .map(|t| t.0),
                                ))
                                .and_then(|s: &str| i64::from_str_radix(&s.replace("_", ""), 2))
                                .message("While parsing a binary Integer")
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let parse_binary_integer { .. } = *self;
                let mut parser = {
                    {
                        string("0b")
                            .with(recognize(
                                (
                                    satisfy(|c: char| c.is_digit(0x2)),
                                    skip_many((
                                        optional(char('_')),
                                        skip_many1(satisfy(|c: char| c.is_digit(0x2))),
                                    )),
                                )
                                    .map(|t| t.0),
                            ))
                            .and_then(|s: &str| i64::from_str_radix(&s.replace("_", ""), 2))
                            .message("While parsing a binary Integer")
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = i64, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let parse_binary_integer { .. } = *self;
                let mut parser = {
                    {
                        string("0b")
                            .with(recognize(
                                (
                                    satisfy(|c: char| c.is_digit(0x2)),
                                    skip_many((
                                        optional(char('_')),
                                        skip_many1(satisfy(|c: char| c.is_digit(0x2))),
                                    )),
                                )
                                    .map(|t| t.0),
                            ))
                            .and_then(|s: &str| i64::from_str_radix(&s.replace("_", ""), 2))
                            .message("While parsing a binary Integer")
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = i64, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn parse_binary_integer<'a, I>() -> parse_binary_integer<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            parse_binary_integer { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct frac<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> &'a str>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for frac<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = &'a str;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<&'a str, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let frac { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            recognize((
                                char('.'),
                                skip_many1(digit()),
                                skip_many((optional(char('_')), skip_many1(digit()))),
                            ))
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let frac { .. } = *self;
                let mut parser = {
                    {
                        recognize((
                            char('.'),
                            skip_many1(digit()),
                            skip_many((optional(char('_')), skip_many1(digit()))),
                        ))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let frac { .. } = *self;
                let mut parser = {
                    {
                        recognize((
                            char('.'),
                            skip_many1(digit()),
                            skip_many((optional(char('_')), skip_many1(digit()))),
                        ))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn frac<'a, I>() -> frac<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            frac { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct exp<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> &'a str>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for exp<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = &'a str;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<&'a str, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let exp { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        { recognize((one_of("eE".chars()), parse_integer())) }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let exp { .. } = *self;
                let mut parser = {
                    { recognize((one_of("eE".chars()), parse_integer())) }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let exp { .. } = *self;
                let mut parser = {
                    { recognize((one_of("eE".chars()), parse_integer())) }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn exp<'a, I>() -> exp<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            exp { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct parse_float<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> &'a str>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for parse_float<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = &'a str;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<&'a str, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let parse_float { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            recognize((
                                attempt((parse_integer(), look_ahead(one_of("eE.".chars())))),
                                choice((exp(), (frac(), optional(exp())).map(|_| ""))),
                            ))
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let parse_float { .. } = *self;
                let mut parser = {
                    {
                        recognize((
                            attempt((parse_integer(), look_ahead(one_of("eE.".chars())))),
                            choice((exp(), (frac(), optional(exp())).map(|_| ""))),
                        ))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let parse_float { .. } = *self;
                let mut parser = {
                    {
                        recognize((
                            attempt((parse_integer(), look_ahead(one_of("eE.".chars())))),
                            choice((exp(), (frac(), optional(exp())).map(|_| ""))),
                        ))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn parse_float<'a, I>() -> parse_float<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            parse_float { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct float<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> f64>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for float<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = f64;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<f64, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let float { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            parse_float()
                                .and_then(|s| s.replace("_", "").parse())
                                .message("While parsing a Float")
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let float { .. } = *self;
                let mut parser = {
                    {
                        parse_float()
                            .and_then(|s| s.replace("_", "").parse())
                            .message("While parsing a Float")
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = f64, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let float { .. } = *self;
                let mut parser = {
                    {
                        parse_float()
                            .and_then(|s| s.replace("_", "").parse())
                            .message("While parsing a Float")
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = f64, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn float<'a, I>() -> float<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            float { __marker: ::combine::lib::marker::PhantomData }
        }
    }
    pub(crate) mod strings {
        use super::super::parser::{
            errors::CustomError,
            trivia::{newline, ws, ws_newlines},
        };
        use combine::{
            char::char,
            error::{Consumed, Info},
            range::{range, take, take_while},
            stream::RangeStream,
            *,
        };
        use std::char;
        #[allow(non_camel_case_types)]
        pub struct string<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> String>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for string<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = String;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<String, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let string { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            choice((
                                ml_basic_string(),
                                basic_string(),
                                ml_literal_string(),
                                literal_string().map(|s: &'a str| s.into()),
                            ))
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let string { .. } = *self;
                let mut parser = {
                    {
                        choice((
                            ml_basic_string(),
                            basic_string(),
                            ml_literal_string(),
                            literal_string().map(|s: &'a str| s.into()),
                        ))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = String, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let string { .. } = *self;
                let mut parser = {
                    {
                        choice((
                            ml_basic_string(),
                            basic_string(),
                            ml_literal_string(),
                            literal_string().map(|s: &'a str| s.into()),
                        ))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = String, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn string<'a, I>() -> string<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            string { __marker: ::combine::lib::marker::PhantomData }
        }
        #[inline]
        fn is_basic_unescaped(c: char) -> bool {
            match c {
                '\u{20}'..='\u{21}' | '\u{23}'..='\u{5B}' | '\u{5D}'..='\u{10FFFF}' => true,
                _ => false,
            }
        }
        #[inline]
        fn is_escape_char(c: char) -> bool {
            match c {
                '\\' | '"' | 'b' | '/' | 'f' | 'n' | 'r' | 't' | 'u' | 'U' => true,
                _ => false,
            }
        }
        #[allow(non_camel_case_types)]
        pub struct escape<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> char>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for escape<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = char;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<char, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let escape { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            satisfy(is_escape_char).message("While parsing escape sequence").then(
                                |c| {
                                    parser(move |input| match c {
                                        'b' => Ok(('\u{8}', Consumed::Empty(()))),
                                        'f' => Ok(('\u{c}', Consumed::Empty(()))),
                                        'n' => Ok(('\n', Consumed::Empty(()))),
                                        'r' => Ok(('\r', Consumed::Empty(()))),
                                        't' => Ok(('\t', Consumed::Empty(()))),
                                        'u' => hexescape(4).parse_stream(input),
                                        'U' => hexescape(8).parse_stream(input),
                                        _ => Ok((c, Consumed::Empty(()))),
                                    })
                                },
                            )
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let escape { .. } = *self;
                let mut parser = {
                    {
                        satisfy(is_escape_char).message("While parsing escape sequence").then(|c| {
                            parser(move |input| match c {
                                'b' => Ok(('\u{8}', Consumed::Empty(()))),
                                'f' => Ok(('\u{c}', Consumed::Empty(()))),
                                'n' => Ok(('\n', Consumed::Empty(()))),
                                'r' => Ok(('\r', Consumed::Empty(()))),
                                't' => Ok(('\t', Consumed::Empty(()))),
                                'u' => hexescape(4).parse_stream(input),
                                'U' => hexescape(8).parse_stream(input),
                                _ => Ok((c, Consumed::Empty(()))),
                            })
                        })
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = char, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let escape { .. } = *self;
                let mut parser = {
                    {
                        satisfy(is_escape_char).message("While parsing escape sequence").then(|c| {
                            parser(move |input| match c {
                                'b' => Ok(('\u{8}', Consumed::Empty(()))),
                                'f' => Ok(('\u{c}', Consumed::Empty(()))),
                                'n' => Ok(('\n', Consumed::Empty(()))),
                                'r' => Ok(('\r', Consumed::Empty(()))),
                                't' => Ok(('\t', Consumed::Empty(()))),
                                'u' => hexescape(4).parse_stream(input),
                                'U' => hexescape(8).parse_stream(input),
                                _ => Ok((c, Consumed::Empty(()))),
                            })
                        })
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = char, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn escape<'a, I>() -> escape<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            escape { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct hexescape<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            pub n: usize,
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> char>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for hexescape<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = char;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<char, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let hexescape { n: ref mut n, .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            take(*n).and_then(|s| u32::from_str_radix(s, 16)).and_then(|h| {
                                char::from_u32(h).ok_or_else(|| CustomError::InvalidHexEscape(h))
                            })
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let hexescape { n: ref mut n, .. } = *self;
                let mut parser = {
                    {
                        take(*n).and_then(|s| u32::from_str_radix(s, 16)).and_then(|h| {
                            char::from_u32(h).ok_or_else(|| CustomError::InvalidHexEscape(h))
                        })
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = char, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let hexescape { n: ref mut n, .. } = *self;
                let mut parser = {
                    {
                        take(*n).and_then(|s| u32::from_str_radix(s, 16)).and_then(|h| {
                            char::from_u32(h).ok_or_else(|| CustomError::InvalidHexEscape(h))
                        })
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = char, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn hexescape<'a, I>(n: usize) -> hexescape<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            hexescape { n: n, __marker: ::combine::lib::marker::PhantomData }
        }
        const ESCAPE: char = '\\';
        #[allow(non_camel_case_types)]
        pub struct basic_char<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> char>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for basic_char<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = char;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<char, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let basic_char { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            satisfy(|c| is_basic_unescaped(c) || c == ESCAPE).then(|c| {
                                parser(move |input| match c {
                                    ESCAPE => escape().parse_stream(input),
                                    _ => Ok((c, Consumed::Empty(()))),
                                })
                            })
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let basic_char { .. } = *self;
                let mut parser = {
                    {
                        satisfy(|c| is_basic_unescaped(c) || c == ESCAPE).then(|c| {
                            parser(move |input| match c {
                                ESCAPE => escape().parse_stream(input),
                                _ => Ok((c, Consumed::Empty(()))),
                            })
                        })
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = char, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let basic_char { .. } = *self;
                let mut parser = {
                    {
                        satisfy(|c| is_basic_unescaped(c) || c == ESCAPE).then(|c| {
                            parser(move |input| match c {
                                ESCAPE => escape().parse_stream(input),
                                _ => Ok((c, Consumed::Empty(()))),
                            })
                        })
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = char, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn basic_char<'a, I>() -> basic_char<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            basic_char { __marker: ::combine::lib::marker::PhantomData }
        }
        const QUOTATION_MARK: char = '"';
        #[allow(non_camel_case_types)]
        pub struct basic_string<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> String>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for basic_string<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = String;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<String, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let basic_string { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            between(char(QUOTATION_MARK), char(QUOTATION_MARK), many(basic_char()))
                                .message("While parsing a Basic String")
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let basic_string { .. } = *self;
                let mut parser = {
                    {
                        between(char(QUOTATION_MARK), char(QUOTATION_MARK), many(basic_char()))
                            .message("While parsing a Basic String")
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = String, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let basic_string { .. } = *self;
                let mut parser = {
                    {
                        between(char(QUOTATION_MARK), char(QUOTATION_MARK), many(basic_char()))
                            .message("While parsing a Basic String")
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = String, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn basic_string<'a, I>() -> basic_string<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            basic_string { __marker: ::combine::lib::marker::PhantomData }
        }
        #[inline]
        fn is_ml_basic_unescaped(c: char) -> bool {
            match c {
                '\u{20}'..='\u{5B}' | '\u{5D}'..='\u{10FFFF}' => true,
                _ => false,
            }
        }
        const ML_BASIC_STRING_DELIM: &str = "\"\"\"";
        #[allow(non_camel_case_types)]
        pub struct ml_basic_char<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> char>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for ml_basic_char<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = char;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<char, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let ml_basic_char { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            satisfy(|c| is_ml_basic_unescaped(c) || c == ESCAPE).then(|c| {
                                parser(move |input| match c {
                                    ESCAPE => escape().parse_stream(input),
                                    _ => Ok((c, Consumed::Empty(()))),
                                })
                            })
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let ml_basic_char { .. } = *self;
                let mut parser = {
                    {
                        satisfy(|c| is_ml_basic_unescaped(c) || c == ESCAPE).then(|c| {
                            parser(move |input| match c {
                                ESCAPE => escape().parse_stream(input),
                                _ => Ok((c, Consumed::Empty(()))),
                            })
                        })
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = char, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let ml_basic_char { .. } = *self;
                let mut parser = {
                    {
                        satisfy(|c| is_ml_basic_unescaped(c) || c == ESCAPE).then(|c| {
                            parser(move |input| match c {
                                ESCAPE => escape().parse_stream(input),
                                _ => Ok((c, Consumed::Empty(()))),
                            })
                        })
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = char, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn ml_basic_char<'a, I>() -> ml_basic_char<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            ml_basic_char { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct try_eat_escaped_newline<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> ()>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for try_eat_escaped_newline<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = ();
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<(), I>
            where
                M: ::combine::parser::ParseMode,
            {
                let try_eat_escaped_newline { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        { skip_many(attempt((char(ESCAPE), ws(), ws_newlines()))) }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let try_eat_escaped_newline { .. } = *self;
                let mut parser = {
                    { skip_many(attempt((char(ESCAPE), ws(), ws_newlines()))) }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = (), PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let try_eat_escaped_newline { .. } = *self;
                let mut parser = {
                    { skip_many(attempt((char(ESCAPE), ws(), ws_newlines()))) }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = (), PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn try_eat_escaped_newline<'a, I>() -> try_eat_escaped_newline<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            try_eat_escaped_newline { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct ml_basic_body<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> String>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for ml_basic_body<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = String;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<String, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let ml_basic_body { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            optional(newline()).skip(try_eat_escaped_newline()).with(many(
                                not_followed_by(range(ML_BASIC_STRING_DELIM).map(Info::Range))
                                    .with(choice((newline(), ml_basic_char())))
                                    .skip(try_eat_escaped_newline()),
                            ))
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let ml_basic_body { .. } = *self;
                let mut parser = {
                    {
                        optional(newline()).skip(try_eat_escaped_newline()).with(many(
                            not_followed_by(range(ML_BASIC_STRING_DELIM).map(Info::Range))
                                .with(choice((newline(), ml_basic_char())))
                                .skip(try_eat_escaped_newline()),
                        ))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = String, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let ml_basic_body { .. } = *self;
                let mut parser = {
                    {
                        optional(newline()).skip(try_eat_escaped_newline()).with(many(
                            not_followed_by(range(ML_BASIC_STRING_DELIM).map(Info::Range))
                                .with(choice((newline(), ml_basic_char())))
                                .skip(try_eat_escaped_newline()),
                        ))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = String, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn ml_basic_body<'a, I>() -> ml_basic_body<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            ml_basic_body { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct ml_basic_string<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> String>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for ml_basic_string<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = String;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<String, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let ml_basic_string { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            between(
                                range(ML_BASIC_STRING_DELIM),
                                range(ML_BASIC_STRING_DELIM),
                                ml_basic_body(),
                            )
                            .message("While parsing a Multiline Basic String")
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let ml_basic_string { .. } = *self;
                let mut parser = {
                    {
                        between(
                            range(ML_BASIC_STRING_DELIM),
                            range(ML_BASIC_STRING_DELIM),
                            ml_basic_body(),
                        )
                        .message("While parsing a Multiline Basic String")
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = String, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let ml_basic_string { .. } = *self;
                let mut parser = {
                    {
                        between(
                            range(ML_BASIC_STRING_DELIM),
                            range(ML_BASIC_STRING_DELIM),
                            ml_basic_body(),
                        )
                        .message("While parsing a Multiline Basic String")
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = String, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn ml_basic_string<'a, I>() -> ml_basic_string<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            ml_basic_string { __marker: ::combine::lib::marker::PhantomData }
        }
        const APOSTROPHE: char = '\'';
        #[inline]
        fn is_literal_char(c: char) -> bool {
            match c {
                '\u{09}' | '\u{20}'..='\u{26}' | '\u{28}'..='\u{10FFFF}' => true,
                _ => false,
            }
        }
        #[allow(non_camel_case_types)]
        pub struct literal_string<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> &'a str>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for literal_string<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = &'a str;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<&'a str, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let literal_string { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            between(char(APOSTROPHE), char(APOSTROPHE), take_while(is_literal_char))
                                .message("While parsing a Literal String")
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let literal_string { .. } = *self;
                let mut parser = {
                    {
                        between(char(APOSTROPHE), char(APOSTROPHE), take_while(is_literal_char))
                            .message("While parsing a Literal String")
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let literal_string { .. } = *self;
                let mut parser = {
                    {
                        between(char(APOSTROPHE), char(APOSTROPHE), take_while(is_literal_char))
                            .message("While parsing a Literal String")
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn literal_string<'a, I>() -> literal_string<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            literal_string { __marker: ::combine::lib::marker::PhantomData }
        }
        const ML_LITERAL_STRING_DELIM: &str = "'''";
        #[inline]
        fn is_ml_literal_char(c: char) -> bool {
            match c {
                '\u{09}' | '\u{20}'..='\u{10FFFF}' => true,
                _ => false,
            }
        }
        #[allow(non_camel_case_types)]
        pub struct ml_literal_body<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> String>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for ml_literal_body<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = String;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<String, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let ml_literal_body { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            optional(newline()).with(many(
                                not_followed_by(range(ML_LITERAL_STRING_DELIM).map(Info::Range))
                                    .with(choice((newline(), satisfy(is_ml_literal_char)))),
                            ))
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let ml_literal_body { .. } = *self;
                let mut parser = {
                    {
                        optional(newline()).with(many(
                            not_followed_by(range(ML_LITERAL_STRING_DELIM).map(Info::Range))
                                .with(choice((newline(), satisfy(is_ml_literal_char)))),
                        ))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = String, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let ml_literal_body { .. } = *self;
                let mut parser = {
                    {
                        optional(newline()).with(many(
                            not_followed_by(range(ML_LITERAL_STRING_DELIM).map(Info::Range))
                                .with(choice((newline(), satisfy(is_ml_literal_char)))),
                        ))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = String, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn ml_literal_body<'a, I>() -> ml_literal_body<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            ml_literal_body { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct ml_literal_string<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> String>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for ml_literal_string<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = String;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<String, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let ml_literal_string { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            between(
                                range(ML_LITERAL_STRING_DELIM),
                                range(ML_LITERAL_STRING_DELIM),
                                ml_literal_body(),
                            )
                            .message("While parsing a Multiline Literal String")
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let ml_literal_string { .. } = *self;
                let mut parser = {
                    {
                        between(
                            range(ML_LITERAL_STRING_DELIM),
                            range(ML_LITERAL_STRING_DELIM),
                            ml_literal_body(),
                        )
                        .message("While parsing a Multiline Literal String")
                    }
                };
                {
                    let _: &mut dyn (::combine::Parser<
                        Input = I,
                        Output = String,
                        PartialState = _,
                    >) = &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let ml_literal_string { .. } = *self;
                let mut parser = {
                    {
                        between(
                            range(ML_LITERAL_STRING_DELIM),
                            range(ML_LITERAL_STRING_DELIM),
                            ml_literal_body(),
                        )
                        .message("While parsing a Multiline Literal String")
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = String, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn ml_literal_string<'a, I>() -> ml_literal_string<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            ml_literal_string { __marker: ::combine::lib::marker::PhantomData }
        }
    }
    pub(crate) mod table {
        use super::super::{
            array_of_tables::ArrayOfTables,
            decor::Decor,
            key::Key,
            parser::{
                errors::CustomError,
                key::key,
                trivia::{line_trailing, ws},
                TomlParser,
            },
            table::{Item, Table},
        };
        use combine::{char::char, range::range, stream::RangeStream, *};
        #[allow(unused_imports)]
        use std::ops::DerefMut;
        use std::{cell::RefCell, mem};
        const TABLE_KEY_SEP: char = '.';
        const STD_TABLE_OPEN: char = '[';
        const STD_TABLE_CLOSE: char = ']';
        const ARRAY_TABLE_OPEN: &str = "[[";
        const ARRAY_TABLE_CLOSE: &str = "]]";
        #[allow(non_camel_case_types)]
        pub struct key_path<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> Vec<Key>>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for key_path<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = Vec<Key>;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Vec<Key>, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let key_path { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            sep_by1(
                                between(ws(), ws(), key().map(|(raw, key)| Key::new(raw, key))),
                                char(TABLE_KEY_SEP),
                            )
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let key_path { .. } = *self;
                let mut parser = {
                    {
                        sep_by1(
                            between(ws(), ws(), key().map(|(raw, key)| Key::new(raw, key))),
                            char(TABLE_KEY_SEP),
                        )
                    }
                };
                {
                    let _: &mut dyn (::combine::Parser<
                        Input = I,
                        Output = Vec<Key>,
                        PartialState = _,
                    >) = &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let key_path { .. } = *self;
                let mut parser = {
                    {
                        sep_by1(
                            between(ws(), ws(), key().map(|(raw, key)| Key::new(raw, key))),
                            char(TABLE_KEY_SEP),
                        )
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = Vec<Key>, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn key_path<'a, I>() -> key_path<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            key_path { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        struct std_table<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            pub parser: &'b RefCell<TomlParser>,
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> ()>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, 'b, I> ::combine::Parser for std_table<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = ();
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<(), I>
            where
                M: ::combine::parser::ParseMode,
            {
                let std_table { parser: ref mut parser, .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            (
                                between(char(STD_TABLE_OPEN), char(STD_TABLE_CLOSE), key_path()),
                                line_trailing(),
                            )
                                .and_then(|(h, t)| {
                                    parser.borrow_mut().deref_mut().on_std_header(&h, t)
                                })
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let std_table { parser: ref mut parser, .. } = *self;
                let mut parser = {
                    {
                        (
                            between(char(STD_TABLE_OPEN), char(STD_TABLE_CLOSE), key_path()),
                            line_trailing(),
                        )
                            .and_then(|(h, t)| parser.borrow_mut().deref_mut().on_std_header(&h, t))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = (), PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let std_table { parser: ref mut parser, .. } = *self;
                let mut parser = {
                    {
                        (
                            between(char(STD_TABLE_OPEN), char(STD_TABLE_CLOSE), key_path()),
                            line_trailing(),
                        )
                            .and_then(|(h, t)| parser.borrow_mut().deref_mut().on_std_header(&h, t))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = (), PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        fn std_table<'a, 'b, I>(parser: &'b RefCell<TomlParser>) -> std_table<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            std_table { parser: parser, __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        struct array_table<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            pub parser: &'b RefCell<TomlParser>,
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> ()>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, 'b, I> ::combine::Parser for array_table<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = ();
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<(), I>
            where
                M: ::combine::parser::ParseMode,
            {
                let array_table { parser: ref mut parser, .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            (
                                between(
                                    range(ARRAY_TABLE_OPEN),
                                    range(ARRAY_TABLE_CLOSE),
                                    key_path(),
                                ),
                                line_trailing(),
                            )
                                .and_then(|(h, t)| {
                                    parser.borrow_mut().deref_mut().on_array_header(&h, t)
                                })
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let array_table { parser: ref mut parser, .. } = *self;
                let mut parser = {
                    {
                        (
                            between(range(ARRAY_TABLE_OPEN), range(ARRAY_TABLE_CLOSE), key_path()),
                            line_trailing(),
                        )
                            .and_then(|(h, t)| {
                                parser.borrow_mut().deref_mut().on_array_header(&h, t)
                            })
                    }
                };
                {
                    let _: &mut dyn (::combine::Parser<Input = I, Output = (), PartialState = _>) =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let array_table { parser: ref mut parser, .. } = *self;
                let mut parser = {
                    {
                        (
                            between(range(ARRAY_TABLE_OPEN), range(ARRAY_TABLE_CLOSE), key_path()),
                            line_trailing(),
                        )
                            .and_then(|(h, t)| {
                                parser.borrow_mut().deref_mut().on_array_header(&h, t)
                            })
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = (), PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        fn array_table<'a, 'b, I>(parser: &'b RefCell<TomlParser>) -> array_table<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            array_table { parser, __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct table<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            pub parser: &'b RefCell<TomlParser>,
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> ()>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, 'b, I> ::combine::Parser for table<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = ();
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<(), I>
            where
                M: ::combine::parser::ParseMode,
            {
                let table { parser: ref mut parser, .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        array_table(parser)
                            .or(std_table(parser))
                            .message("While parsing a Table Header")
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let table { parser: ref mut parser, .. } = *self;
                let mut parser = {
                    array_table(parser)
                        .or(std_table(parser))
                        .message("While parsing a Table Header")
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = (), PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let table { parser: ref mut parser, .. } = *self;
                let mut parser = {
                    array_table(parser)
                        .or(std_table(parser))
                        .message("While parsing a Table Header")
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = (), PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn table<'a, 'b, I>(parser: &'b RefCell<TomlParser>) -> table<'a, 'b, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            table { parser, __marker: ::combine::lib::marker::PhantomData }
        }
        pub(crate) fn duplicate_key(path: &[Key], i: usize) -> CustomError {
            assert!(i < path.len());
            let header: Vec<&str> = path[..i].iter().map(Key::raw).collect();
            CustomError::DuplicateKey {
                key: path[i].raw().into(),
                table: format!("[{}]", header.join(".")),
            }
        }
        impl TomlParser {
            pub(crate) fn descend_path<'a>(
                table: &'a mut Table,
                path: &[Key],
                i: usize,
            ) -> Result<&'a mut Table, CustomError> {
                if let Some(key) = path.get(i) {
                    let entry = table.entry(key.raw());
                    if entry.is_none() {
                        let mut new_table = Table::new();
                        new_table.set_implicit(true);
                        *entry = Item::Table(new_table);
                    }
                    match *entry {
                        Item::Value(..) => Err(duplicate_key(path, i)),
                        Item::ArrayOfTables(ref mut array) => {
                            debug_assert!(!array.is_empty());
                            let index = array.len() - 1;
                            let last_child = array.get_mut(index).unwrap();
                            Self::descend_path(last_child, path, i + 1)
                        }
                        Item::Table(ref mut sweet_child_of_mine) => {
                            TomlParser::descend_path(sweet_child_of_mine, path, i + 1)
                        }
                        _ => unreachable!(),
                    }
                } else {
                    Ok(table)
                }
            }
            fn on_std_header(&mut self, path: &[Key], trailing: &str) -> Result<(), CustomError> {
                debug_assert!(!path.is_empty());

                let leading = mem::replace(&mut self.document.trailing, String::new());
                let table = self.document.as_table_mut();
                self.current_table_position += 1;
                let table = Self::descend_path(table, &path[..path.len() - 1], 0);
                let key = &path[path.len() - 1];
                match table {
                    Ok(table) => {
                        let decor = Decor::new(leading, trailing.into());
                        let entry = table.entry(key.raw());
                        if entry.is_none() {
                            *entry = Item::Table(Table::with_decor_and_pos(
                                decor,
                                Some(self.current_table_position),
                            ));
                            self.current_table_path = path.to_vec();
                            return Ok(());
                        }
                        match *entry {
                            Item::Table(ref mut t) if t.implicit => {
                                debug_assert!(t.values_len() == 0);
                                t.decor = decor;
                                t.position = Some(self.current_table_position);
                                t.set_implicit(false);
                                self.current_table_path = path.to_vec();
                                return Ok(());
                            }
                            _ => {}
                        }
                        Err(duplicate_key(&path[..], path.len() - 1))
                    }
                    Err(e) => Err(e),
                }
            }
            fn on_array_header(&mut self, path: &[Key], trailing: &str) -> Result<(), CustomError> {
                debug_assert!(!path.is_empty());
                let leading = mem::replace(&mut self.document.trailing, String::new());
                let table = self.document.as_table_mut();
                let key = &path[path.len() - 1];
                let table = Self::descend_path(table, &path[..path.len() - 1], 0);
                match table {
                    Ok(table) => {
                        if !table.contains_table(key.get()) && !table.contains_value(key.get()) {
                            let decor = Decor::new(leading, trailing.into());
                            let entry = table
                                .entry(key.raw())
                                .or_insert(Item::ArrayOfTables(ArrayOfTables::new()));
                            let array = entry.as_array_of_tables_mut().unwrap();
                            self.current_table_position += 1;
                            array.append(Table::with_decor_and_pos(
                                decor,
                                Some(self.current_table_position),
                            ));
                            self.current_table_path = path.to_vec();
                            Ok(())
                        } else {
                            Err(duplicate_key(&path[..], path.len() - 1))
                        }
                    }
                    Err(e) => Err(e),
                }
            }
        }
    }
    pub(crate) mod trivia {
        use combine::{
            char::{char, crlf, newline as lf},
            range::{recognize, take_while, take_while1},
            stream::RangeStream,
            *,
        };
        #[inline]
        fn is_wschar(c: char) -> bool {
            match c {
                ' ' | '\t' => true,
                _ => false,
            }
        }
        #[allow(non_camel_case_types)]
        pub struct ws<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> &'a str>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for ws<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = &'a str;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<&'a str, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let ws { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        { take_while(is_wschar) }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let ws { .. } = *self;
                let mut parser = {
                    { take_while(is_wschar) }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let ws { .. } = *self;
                let mut parser = {
                    { take_while(is_wschar) }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn ws<'a, I>() -> ws<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            ws { __marker: ::combine::lib::marker::PhantomData }
        }
        #[inline]
        fn is_non_eol(c: char) -> bool {
            match c {
                '\u{09}' | '\u{20}'..='\u{10FFFF}' => true,
                _ => false,
            }
        }
        const COMMENT_START_SYMBOL: char = '#';
        #[allow(non_camel_case_types)]
        pub struct comment<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> &'a str>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for comment<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = &'a str;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<&'a str, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let comment { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        { recognize((attempt(char(COMMENT_START_SYMBOL)), take_while(is_non_eol))) }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let comment { .. } = *self;
                let mut parser = {
                    { recognize((attempt(char(COMMENT_START_SYMBOL)), take_while(is_non_eol))) }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let comment { .. } = *self;
                let mut parser = {
                    { recognize((attempt(char(COMMENT_START_SYMBOL)), take_while(is_non_eol))) }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn comment<'a, I>() -> comment<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            comment { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct newline<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> char>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for newline<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = char;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<char, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let newline { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        { choice((lf(), crlf())).map(|_| '\n').expected("a newline") }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let newline { .. } = *self;
                let mut parser = {
                    { choice((lf(), crlf())).map(|_| '\n').expected("a newline") }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = char, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let newline { .. } = *self;
                let mut parser = {
                    { choice((lf(), crlf())).map(|_| '\n').expected("a newline") }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = char, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn newline<'a, I>() -> newline<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            newline { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct ws_newline<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> &'a str>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for ws_newline<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = &'a str;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<&'a str, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let ws_newline { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            recognize(skip_many(choice((
                                newline().map(|_| "\n"),
                                take_while1(is_wschar),
                            ))))
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let ws_newline { .. } = *self;
                let mut parser = {
                    {
                        recognize(skip_many(choice((
                            newline().map(|_| "\n"),
                            take_while1(is_wschar),
                        ))))
                    }
                };
                {
                    let _: &mut dyn (::combine::Parser<
                        Input = I,
                        Output = &'a str,
                        PartialState = _,
                    >) = &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let ws_newline { .. } = *self;
                let mut parser = {
                    {
                        recognize(skip_many(choice((
                            newline().map(|_| "\n"),
                            take_while1(is_wschar),
                        ))))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn ws_newline<'a, I>() -> ws_newline<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            ws_newline { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct ws_newlines<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> &'a str>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for ws_newlines<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = &'a str;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<&'a str, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let ws_newlines { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        { recognize((newline(), ws_newline())) }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let ws_newlines { .. } = *self;
                let mut parser = {
                    { recognize((newline(), ws_newline())) }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let ws_newlines { .. } = *self;
                let mut parser = {
                    { recognize((newline(), ws_newline())) }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn ws_newlines<'a, I>() -> ws_newlines<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            ws_newlines { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct ws_comment_newline<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> &'a str>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for ws_comment_newline<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = &'a str;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<&'a str, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let ws_comment_newline { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            recognize(skip_many(choice((
                                skip_many1(choice((
                                    take_while1(is_wschar),
                                    newline().map(|_| "\n"),
                                ))),
                                comment().map(|_| ()),
                            ))))
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let ws_comment_newline { .. } = *self;
                let mut parser = {
                    {
                        recognize(skip_many(choice((
                            skip_many1(choice((take_while1(is_wschar), newline().map(|_| "\n")))),
                            comment().map(|_| ()),
                        ))))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let ws_comment_newline { .. } = *self;
                let mut parser = {
                    {
                        recognize(skip_many(choice((
                            skip_many1(choice((take_while1(is_wschar), newline().map(|_| "\n")))),
                            comment().map(|_| ()),
                        ))))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn ws_comment_newline<'a, I>() -> ws_comment_newline<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            ws_comment_newline { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct line_ending<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> &'a str>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for line_ending<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = &'a str;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<&'a str, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let line_ending { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        { choice((newline().map(|_| "\n"), eof().map(|_| ""))) }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let line_ending { .. } = *self;
                let mut parser = {
                    { choice((newline().map(|_| "\n"), eof().map(|_| ""))) }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let line_ending { .. } = *self;
                let mut parser = {
                    { choice((newline().map(|_| "\n"), eof().map(|_| ""))) }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn line_ending<'a, I>() -> line_ending<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            line_ending { __marker: ::combine::lib::marker::PhantomData }
        }
        #[allow(non_camel_case_types)]
        pub struct line_trailing<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> &'a str>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for line_trailing<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = &'a str;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<&'a str, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let line_trailing { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        { recognize((ws(), optional(comment()))).skip(line_ending()) }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let line_trailing { .. } = *self;
                let mut parser = {
                    { recognize((ws(), optional(comment()))).skip(line_ending()) }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let line_trailing { .. } = *self;
                let mut parser = {
                    { recognize((ws(), optional(comment()))).skip(line_ending()) }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = &'a str, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn line_trailing<'a, I>() -> line_trailing<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            line_trailing { __marker: ::combine::lib::marker::PhantomData }
        }
    }
    pub(crate) mod value {
        use super::super::{
            decor::{Formatted, Repr},
            formatted,
            parser::{
                array::array,
                datetime::date_time,
                inline_table::inline_table,
                numbers::{boolean, float, integer},
                strings::string,
            },
            value as v,
        };
        use combine::{range::recognize_with_value, stream::RangeStream, *};
        #[allow(non_camel_case_types)]
        pub struct value<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            __marker: ::combine::lib::marker::PhantomData<fn(I) -> v::Value>,
        }
        #[allow(non_shorthand_field_patterns)]
        impl<'a, I> ::combine::Parser for value<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            type Input = I;
            type Output = v::Value;
            type PartialState = ();
            #[inline]
            fn parse_partial(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::PartialMode::default(), input, state)
            }
            #[inline]
            fn parse_first(
                &mut self,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<Self::Output, Self::Input> {
                self.parse_mode(::combine::parser::FirstMode, input, state)
            }
            #[inline]
            fn parse_mode_impl<M>(
                &mut self,
                mode: M,
                input: &mut Self::Input,
                state: &mut Self::PartialState,
            ) -> ::combine::error::ConsumedResult<v::Value, I>
            where
                M: ::combine::parser::ParseMode,
            {
                let value { .. } = *self;
                {
                    let _ = state;
                    let mut state = Default::default();
                    let state = &mut state;
                    {
                        {
                            recognize_with_value(choice((
                                string().map(|s| {
                                    v::Value::String(Formatted::new(
                                        s,
                                        Repr::new(
                                            "".to_string(),
                                            "who cares?".into(),
                                            "".to_string(),
                                        ),
                                    ))
                                }),
                                boolean().map(v::Value::from),
                                array().map(v::Value::Array),
                                inline_table().map(v::Value::InlineTable),
                                date_time().map(v::Value::from),
                                float().map(v::Value::from),
                                integer().map(v::Value::from),
                            )))
                            .map(|(raw, value)| formatted::value(value, raw))
                        }
                    }
                    .parse_mode(mode, input, state)
                }
            }
            #[inline]
            fn add_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let value { .. } = *self;
                let mut parser = {
                    {
                        recognize_with_value(choice((
                            string().map(|s| {
                                v::Value::String(Formatted::new(
                                    s,
                                    Repr::new("".to_string(), "who cares?".into(), "".to_string()),
                                ))
                            }),
                            boolean().map(v::Value::from),
                            array().map(v::Value::Array),
                            inline_table().map(v::Value::InlineTable),
                            date_time().map(v::Value::from),
                            float().map(v::Value::from),
                            integer().map(v::Value::from),
                        )))
                        .map(|(raw, value)| formatted::value(value, raw))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = v::Value, PartialState = _> =
                        &mut parser;
                }
                parser.add_error(errors)
            }
            fn add_consumed_expected_error(
                &mut self,
                errors: &mut ::combine::error::Tracked<<I as ::combine::stream::StreamOnce>::Error>,
            ) {
                let value { .. } = *self;
                let mut parser = {
                    {
                        recognize_with_value(choice((
                            string().map(|s| {
                                v::Value::String(Formatted::new(
                                    s,
                                    Repr::new("".to_string(), "who cares?".into(), "".to_string()),
                                ))
                            }),
                            boolean().map(v::Value::from),
                            array().map(v::Value::Array),
                            inline_table().map(v::Value::InlineTable),
                            date_time().map(v::Value::from),
                            float().map(v::Value::from),
                            integer().map(v::Value::from),
                        )))
                        .map(|(raw, value)| formatted::value(value, raw))
                    }
                };
                {
                    let _: &mut ::combine::Parser<Input = I, Output = v::Value, PartialState = _> =
                        &mut parser;
                }
                parser.add_consumed_expected_error(errors)
            }
        }
        #[inline]
        pub fn value<'a, I>() -> value<'a, I>
        where
            <I as ::combine::stream::StreamOnce>::Error: ::combine::error::ParseError<
                <I as ::combine::stream::StreamOnce>::Item,
                <I as ::combine::stream::StreamOnce>::Range,
                <I as ::combine::stream::StreamOnce>::Position,
            >,
            I: RangeStream<Range = &'a str, Item = char>,
            I::Error: ParseError<char, &'a str, <I as StreamOnce>::Position>,
            <I::Error as ParseError<char, &'a str, <I as StreamOnce>::Position>>::StreamError: From<std::num::ParseIntError>
                + From<std::num::ParseFloatError>
                + From<chrono::ParseError>
                + From<crate::parser::errors::CustomError>,
        {
            value { __marker: ::combine::lib::marker::PhantomData }
        }
    }
    pub use self::errors::TomlError;
    pub(crate) use self::{key::key as key_parser, value::value as value_parser};
    use super::{document::Document, key::Key};
    pub struct TomlParser {
        document: Box<Document>,
        current_table_path: Vec<Key>,
        current_table_position: usize,
    }
    impl Default for TomlParser {
        fn default() -> Self {
            Self {
                document: Box::new(Document::new()),
                current_table_path: Vec::new(),
                current_table_position: 0,
            }
        }
    }
}
pub(crate) mod table {
    use super::{
        array_of_tables::ArrayOfTables,
        decor::{Decor, Repr},
        formatted::{decorated, key_repr},
        key::Key,
        value::{sort_key_value_pairs, Array, DateTime, InlineTable, Value},
    };
    use linked_hash_map::LinkedHashMap;
    /// Type representing a TOML non-inline table
    #[derive(Clone, Debug, Default)]
    pub struct Table {
        pub(crate) items: KeyValuePairs,
        pub(crate) decor: Decor,
        pub(crate) implicit: bool,
        pub(crate) position: Option<usize>,
    }
    pub(crate) type KeyValuePairs = LinkedHashMap<String, TableKeyValue>;
    /// Type representing either a value, a table, an array of tables, or none.
    #[derive(Debug, Clone)]
    pub enum Item {
        /// Type representing none.
        None,
        /// Type representing value.
        Value(Value),
        /// Type representing table.
        Table(Table),
        /// Type representing array of tables.
        ArrayOfTables(ArrayOfTables),
    }
    impl Default for Item {
        fn default() -> Self {
            Item::None
        }
    }
    #[derive(Debug, Clone)]
    #[doc(hidden)]
    pub struct TableKeyValue {
        pub(crate) key: Repr,
        pub(crate) value: Item,
    }
    impl TableKeyValue {
        pub(crate) fn new(key: Repr, value: Item) -> Self {
            TableKeyValue { key, value }
        }
    }
    /// An iterator type over `Table`'s key/value pairs.
    pub type Iter<'a> = Box<dyn Iterator<Item = (&'a str, &'a Item)> + 'a>;
    pub type IterMut<'a> = Box<dyn Iterator<Item = (&'a str, &'a mut Item)> + 'a>;
    impl Table {
        /// Creates an empty table.
        pub fn new() -> Self {
            Self::with_decor_and_pos(Decor::new("\n", ""), None)
        }
        pub(crate) fn with_pos(position: Option<usize>) -> Self {
            Self { position, ..Default::default() }
        }
        pub(crate) fn with_decor_and_pos(decor: Decor, position: Option<usize>) -> Self {
            Self { decor, position, ..Default::default() }
        }
        /// Returns true iff the table contains an item with the given key.
        pub fn contains_key(&self, key: &str) -> bool {
            if let Some(kv) = self.items.get(key) { !kv.value.is_none() } else { false }
        }
        /// Returns true iff the table contains a table with the given key.
        pub fn contains_table(&self, key: &str) -> bool {
            if let Some(kv) = self.items.get(key) { kv.value.is_table() } else { false }
        }
        /// Returns true iff the table contains a value with the given key.
        pub fn contains_value(&self, key: &str) -> bool {
            if let Some(kv) = self.items.get(key) { kv.value.is_value() } else { false }
        }
        /// Returns true iff the table contains an array of tables with the given key.
        pub fn contains_array_of_tables(&self, key: &str) -> bool {
            if let Some(kv) = self.items.get(key) { kv.value.is_array_of_tables() } else { false }
        }
        /// Returns an iterator over all key/value pairs, including empty.
        pub fn iter(&self) -> Iter<'_> {
            Box::new(self.items.iter().map(|(key, kv)| (&key[..], &kv.value)))
        }
        /// Returns an iterator over all key/value pairs, including empty.
        pub fn iter_mut(&mut self) -> IterMut<'_> {
            Box::new(self.items.iter_mut().map(|(key, kv)| (&key[..], &mut kv.value)))
        }
        /// Removes an item given the key.
        pub fn remove(&mut self, key: &str) -> Option<Item> {
            self.items.remove(key).map(|kv| kv.value)
        }
        /// Sorts Key/Value Pairs of the table,
        /// doesn't affect subtables or subarrays.
        pub fn sort_values(&mut self) {
            sort_key_value_pairs(&mut self.items);
        }
        /// Returns the number of non-empty items in the table.
        pub fn len(&self) -> usize {
            self.items.iter().filter(|i| !(i.1).value.is_none()).count()
        }
        /// Returns the number of key/value pairs in the table.
        pub fn values_len(&self) -> usize {
            self.items.iter().filter(|i| (i.1).value.is_value()).count()
        }
        /// Returns true iff the table is empty.
        pub fn is_empty(&self) -> bool {
            self.len() == 0
        }
        /// Given the `key`, return a mutable reference to the value.
        /// If there is no entry associated with the given key in the table,
        /// a `Item::None` value will be inserted.
        ///
        /// To insert to table, use `entry` to return a mutable reference
        /// and set it to the appropriate value.
        pub fn entry<'a>(&'a mut self, key: &str) -> &'a mut Item {
            let parsed_key = key.parse::<Key>().expect("invalid key");
            &mut self
                .items
                .entry(parsed_key.get().to_owned())
                .or_insert(TableKeyValue::new(key_repr(parsed_key.raw()), Item::None))
                .value
        }
        /// Returns an optional reference to an item given the key.
        pub fn get<'a>(&'a self, key: &str) -> Option<&'a Item> {
            self.items.get(key).map(|kv| &kv.value)
        }
        /// If a table has no key/value pairs and implicit, it will not be displayed.
        ///
        /// # Examples
        ///
        /// ```notrust
        /// [target."x86_64/windows.json".dependencies]
        /// ```
        ///
        /// In the document above, tables `target` and `target."x86_64/windows.json"` are implicit.
        ///
        /// ```
        /// # extern crate toml_edit;
        /// # use toml_edit::Document;
        /// #
        /// # fn main() {
        /// let mut doc = "[a]\n[a.b]\n".parse::<Document>().expect("invalid toml");
        ///
        /// doc["a"].as_table_mut().unwrap().set_implicit(true);
        /// assert_eq!(doc.to_string(), "[a.b]\n");
        /// # }
        /// ```
        pub fn set_implicit(&mut self, implicit: bool) {
            self.implicit = implicit;
        }
    }
    impl Item {
        /// Sets `self` to the given item iff `self` is none and
        /// returns a mutable reference to `self`.
        pub fn or_insert(&mut self, item: Item) -> &mut Item {
            if self.is_none() {
                *self = item
            }
            self
        }
    }
    /// Downcasting
    impl Item {
        /// Casts `self` to value.
        pub fn as_value(&self) -> Option<&Value> {
            match *self {
                Item::Value(ref v) => Some(v),
                _ => None,
            }
        }
        /// Casts `self` to table.
        pub fn as_table(&self) -> Option<&Table> {
            match *self {
                Item::Table(ref t) => Some(t),
                _ => None,
            }
        }
        /// Casts `self` to array of tables.
        pub fn as_array_of_tables(&self) -> Option<&ArrayOfTables> {
            match *self {
                Item::ArrayOfTables(ref a) => Some(a),
                _ => None,
            }
        }
        /// Casts `self` to mutable value.
        pub fn as_value_mut(&mut self) -> Option<&mut Value> {
            match *self {
                Item::Value(ref mut v) => Some(v),
                _ => None,
            }
        }
        /// Casts `self` to mutable table.
        pub fn as_table_mut(&mut self) -> Option<&mut Table> {
            match *self {
                Item::Table(ref mut t) => Some(t),
                _ => None,
            }
        }
        /// Casts `self` to mutable array of tables.
        pub fn as_array_of_tables_mut(&mut self) -> Option<&mut ArrayOfTables> {
            match *self {
                Item::ArrayOfTables(ref mut a) => Some(a),
                _ => None,
            }
        }
        /// Returns true iff `self` is a value.
        pub fn is_value(&self) -> bool {
            self.as_value().is_some()
        }
        /// Returns true iff `self` is a table.
        pub fn is_table(&self) -> bool {
            self.as_table().is_some()
        }
        /// Returns true iff `self` is an array of tables.
        pub fn is_array_of_tables(&self) -> bool {
            self.as_array_of_tables().is_some()
        }
        /// Returns true iff `self` is `None`.
        pub fn is_none(&self) -> bool {
            match *self {
                Item::None => true,
                _ => false,
            }
        }
        /// Casts `self` to integer.
        pub fn as_integer(&self) -> Option<i64> {
            self.as_value().and_then(Value::as_integer)
        }
        /// Returns true iff `self` is an integer.
        pub fn is_integer(&self) -> bool {
            self.as_integer().is_some()
        }
        /// Casts `self` to float.
        pub fn as_float(&self) -> Option<f64> {
            self.as_value().and_then(Value::as_float)
        }
        /// Returns true iff `self` is a float.
        pub fn is_float(&self) -> bool {
            self.as_float().is_some()
        }
        /// Casts `self` to boolean.
        pub fn as_bool(&self) -> Option<bool> {
            self.as_value().and_then(Value::as_bool)
        }
        /// Returns true iff `self` is a boolean.
        pub fn is_bool(&self) -> bool {
            self.as_bool().is_some()
        }
        /// Casts `self` to str.
        pub fn as_str(&self) -> Option<&str> {
            self.as_value().and_then(Value::as_str)
        }
        /// Returns true iff `self` is a string.
        pub fn is_str(&self) -> bool {
            self.as_str().is_some()
        }
        /// Casts `self` to date-time.
        pub fn as_date_time(&self) -> Option<&DateTime> {
            self.as_value().and_then(Value::as_date_time)
        }
        /// Returns true iff `self` is a date-time.
        pub fn is_date_time(&self) -> bool {
            self.as_date_time().is_some()
        }
        /// Casts `self` to array.
        pub fn as_array(&self) -> Option<&Array> {
            self.as_value().and_then(Value::as_array)
        }
        /// Casts `self` to mutable array.
        pub fn as_array_mut(&mut self) -> Option<&mut Array> {
            self.as_value_mut().and_then(Value::as_array_mut)
        }
        /// Returns true iff `self` is an array.
        pub fn is_array(&self) -> bool {
            self.as_array().is_some()
        }
        /// Casts `self` to inline table.
        pub fn as_inline_table(&self) -> Option<&InlineTable> {
            self.as_value().and_then(Value::as_inline_table)
        }
        /// Casts `self` to mutable inline table.
        pub fn as_inline_table_mut(&mut self) -> Option<&mut InlineTable> {
            self.as_value_mut().and_then(Value::as_inline_table_mut)
        }
        /// Returns true iff `self` is an inline table.
        pub fn is_inline_table(&self) -> bool {
            self.as_inline_table().is_some()
        }
        /// Casts `self` to either a table or an inline table.
        pub fn as_table_like(&self) -> Option<&dyn TableLike> {
            self.as_table()
                .map(|t| t as &dyn TableLike)
                .or_else(|| self.as_inline_table().map(|t| t as &dyn TableLike))
        }
        /// Returns true iff `self` is either a table, or an inline table.
        pub fn is_table_like(&self) -> bool {
            self.as_table_like().is_some()
        }
    }
    /// This trait represents either a `Table`, or an `InlineTable`.
    pub trait TableLike {
        /// Returns an iterator over key/value pairs.
        fn iter(&self) -> Iter;
        /// Returns the number of nonempty items.
        fn len(&self) -> usize {
            self.iter().filter(|&(_, v)| !v.is_none()).count()
        }
        /// Returns true iff the table is empty.
        fn is_empty(&self) -> bool {
            self.len() == 0
        }
        /// Returns an optional reference to an item given the key.
        fn get<'s>(&'s self, key: &str) -> Option<&'s Item>;
    }
    impl TableLike for Table {
        /// Returns an iterator over all subitems, including `Item::None`.
        fn iter(&self) -> Iter {
            self.iter()
        }
        fn get<'s>(&'s self, key: &str) -> Option<&'s Item> {
            self.get(key)
        }
    }
    /// Returns a formatted value.
    ///
    /// Since formatting is part of a `Value`, the right hand side of the
    /// assignment needs to be decorated with a space before the value.
    /// The `value` function does just that.
    ///
    /// # Examples
    /// ```rust
    /// # extern crate toml_edit;
    /// # extern crate pretty_assertions;
    /// # use pretty_assertions::assert_eq;
    /// # use toml_edit::*;
    /// # fn main() {
    /// let mut table = Table::default();
    /// let mut array = Array::default();
    /// array.push("hello");
    /// array.push("\\, world"); // \ is only allowed in a literal string
    /// table["key1"] = value("value1");
    /// table["key2"] = value(42);
    /// table["key3"] = value(array);
    /// assert_eq!(table.to_string(),
    /// r#"key1 = "value1"
    /// key2 = 42
    /// key3 = ["hello", '\, world']
    /// "#);
    /// # }
    /// ```
    pub fn value<V: Into<Value>>(v: V) -> Item {
        Item::Value(decorated(v.into(), " ", ""))
    }
    /// Returns an empty table.
    pub fn table() -> Item {
        Item::Table(Table::new())
    }
    /// Returns an empty array of tables.
    pub fn array() -> Item {
        Item::ArrayOfTables(ArrayOfTables::new())
    }
}
pub(crate) mod value {
    use super::{
        decor::{Decor, Formatted},
        formatted,
        key::Key,
        parser,
        table::{Item, Iter, KeyValuePairs, TableKeyValue, TableLike},
    };
    use chrono::{self, FixedOffset};
    use combine::stream::state::State;
    use linked_hash_map::LinkedHashMap;
    use std::{mem, str::FromStr};
    /// Representation of a TOML Value (as part of a Key/Value Pair).
    #[derive(Debug, Clone)]
    pub enum Value {
        /// A 64-bit integer value.
        Integer(Formatted<i64>),
        /// A string value.
        String(Formatted<String>),
        /// A 64-bit float value.
        Float(Formatted<f64>),
        /// A Date-Time value.
        DateTime(Formatted<DateTime>),
        /// A boolean value.
        Boolean(Formatted<bool>),
        /// An inline array of values.
        Array(Array),
        /// An inline table of key/value pairs.
        InlineTable(InlineTable),
    }
    /// Type representing a TOML Date-Time,
    /// payload of the `Value::DateTime` variant's value
    #[derive(Eq, PartialEq, Clone, Debug, Hash)]
    pub enum DateTime {
        /// An RFC 3339 formatted date-time with offset.
        OffsetDateTime(chrono::DateTime<FixedOffset>),
        /// An RFC 3339 formatted date-time without offset.
        LocalDateTime(chrono::NaiveDateTime),
        /// Date portion of an RFC 3339 formatted date-time.
        LocalDate(chrono::NaiveDate),
        /// Time portion of an RFC 3339 formatted date-time.
        LocalTime(chrono::NaiveTime),
    }
    /// Type representing a TOML array,
    /// payload of the `Value::Array` variant's value
    #[derive(Debug, Default, Clone)]
    pub struct Array {
        pub(crate) values: Vec<Item>,
        pub(crate) trailing: String,
        pub(crate) trailing_comma: bool,
        pub(crate) decor: Decor,
    }
    /// Type representing a TOML inline table,
    /// payload of the `Value::InlineTable` variant
    #[derive(Debug, Default, Clone)]
    pub struct InlineTable {
        pub(crate) items: KeyValuePairs,
        pub(crate) preamble: String,
        pub(crate) decor: Decor,
    }
    #[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
    pub(crate) enum ValueType {
        None,
        Integer,
        String,
        Float,
        DateTime,
        Boolean,
        Array,
        InlineTable,
    }
    /// An iterator type over `Array`'s values.
    pub type ArrayIter<'a> = Box<dyn Iterator<Item = &'a Value> + 'a>;
    impl Array {
        /// Returns the length of the underlying Vec.
        /// To get the actual number of items use `a.iter().count()`.
        pub fn len(&self) -> usize {
            self.values.len()
        }
        /// Return true iff `self.len() == 0`.
        pub fn is_empty(&self) -> bool {
            self.len() == 0
        }
        /// Returns an iterator over all values.
        pub fn iter(&self) -> ArrayIter<'_> {
            Box::new(self.values.iter().filter_map(Item::as_value))
        }
        /// Appends a new value.
        pub fn push<V: Into<Value>>(&mut self, v: V) -> bool {
            self.push_value(v.into(), true)
        }
        /// Return an optional reference to the value at the given index.
        pub fn get(&mut self, index: usize) -> Option<&Value> {
            self.values.get(index).and_then(Item::as_value)
        }
        /// Removes the value at the given index.
        pub fn remove(&mut self, index: usize) -> Value {
            let removed = self.values.remove(index);
            if self.is_empty() {
                self.trailing_comma = false;
            }
            match removed {
                Item::Value(v) => v,
                x => panic!("non-value item {:?} in an array", x),
            }
        }
        /// Auto formats the array.
        pub fn fmt(&mut self) {
            formatted::decorate_array(self);
        }
        pub(crate) fn push_value(&mut self, v: Value, decorate: bool) -> bool {
            let mut value = v;
            if !self.is_empty() && decorate {
                formatted::decorate(&mut value, " ", "");
            } else if decorate {
                formatted::decorate(&mut value, "", "");
            }
            if self.is_empty() || value.get_type() == self.value_type() {
                self.values.push(Item::Value(value));
                true
            } else {
                false
            }
        }
        pub(crate) fn value_type(&self) -> ValueType {
            if let Some(value) = self.values.get(0).and_then(Item::as_value) {
                value.get_type()
            } else {
                ValueType::None
            }
        }
    }
    /// An iterator type over key/value pairs of an inline table.
    pub type InlineTableIter<'a> = Box<dyn Iterator<Item = (&'a str, &'a Value)> + 'a>;
    impl InlineTable {
        /// Returns the number of key/value pairs.
        pub fn len(&self) -> usize {
            self.iter().count()
        }
        /// Returns true iff the table is empty.
        pub fn is_empty(&self) -> bool {
            self.len() == 0
        }
        /// Returns an iterator over key/value pairs.
        pub fn iter(&self) -> InlineTableIter<'_> {
            Box::new(
                self.items
                    .iter()
                    .filter(|&(_, kv)| kv.value.is_value())
                    .map(|(k, kv)| (&k[..], kv.value.as_value().unwrap())),
            )
        }
        /// Sorts the key/value pairs by key.
        pub fn sort(&mut self) {
            sort_key_value_pairs(&mut self.items);
        }
        /// Returns true iff the table contains given key.
        pub fn contains_key(&self, key: &str) -> bool {
            if let Some(kv) = self.items.get(key) { !kv.value.is_none() } else { false }
        }
        /// Merges the key/value pairs into the `other` table leaving
        /// `self` empty.
        pub fn merge_into(&mut self, other: &mut InlineTable) {
            let items = mem::replace(&mut self.items, KeyValuePairs::new());
            for (k, kv) in items {
                other.items.insert(k, kv);
            }
        }
        /// Inserts a key/value pair if the table does not contain the key.
        /// Returns a mutable reference to the corresponding value.
        pub fn get_or_insert<V: Into<Value>>(&mut self, key: &str, value: V) -> &mut Value {
            let parsed = key.parse::<Key>().expect("invalid key");
            self.items
                .entry(parsed.get().to_owned())
                .or_insert(formatted::to_key_value(key, value.into()))
                .value
                .as_value_mut()
                .expect("non-value type in inline table")
        }
        /// Auto formats the table.
        pub fn fmt(&mut self) {
            formatted::decorate_inline_table(self);
        }
        /// Removes a key/value pair given the key.
        pub fn remove(&mut self, key: &str) -> Option<Value> {
            self.items.remove(key).and_then(|kv| kv.value.as_value().cloned())
        }
        /// Return an optional reference to the value at the given the key.
        pub fn get(&self, key: &str) -> Option<&Value> {
            self.items.get(key).and_then(|kv| kv.value.as_value())
        }
        /// Return an optional mutable reference to the value at the given the key.
        pub fn get_mut(&mut self, key: &str) -> Option<&mut Value> {
            self.items.get_mut(key).and_then(|kv| kv.value.as_value_mut())
        }
    }
    impl TableLike for InlineTable {
        fn iter(&self) -> Iter<'_> {
            Box::new(self.items.iter().map(|(key, kv)| (&key[..], &kv.value)))
        }
        fn get<'s>(&'s self, key: &str) -> Option<&'s Item> {
            self.items.get(key).map(|kv| &kv.value)
        }
    }
    /// Downcasting
    impl DateTime {
        /// Casts `self` to offset date-time.
        pub fn as_offset_date_time(&self) -> Option<&chrono::DateTime<FixedOffset>> {
            match *self {
                DateTime::OffsetDateTime(ref dt) => Some(dt),
                _ => None,
            }
        }
        /// Casts `self` to local date-time.
        pub fn as_local_date_time(&self) -> Option<&chrono::NaiveDateTime> {
            match *self {
                DateTime::LocalDateTime(ref dt) => Some(dt),
                _ => None,
            }
        }
        /// Casts `self` to local date.
        pub fn as_local_date(&self) -> Option<&chrono::NaiveDate> {
            match *self {
                DateTime::LocalDate(ref d) => Some(d),
                _ => None,
            }
        }
        /// Casts `self` to local time.
        pub fn as_local_time(&self) -> Option<&chrono::NaiveTime> {
            match *self {
                DateTime::LocalTime(ref t) => Some(t),
                _ => None,
            }
        }
        /// Returns true iff `self` is an offset date-time.
        pub fn is_offset_date_time(&self) -> bool {
            self.as_offset_date_time().is_some()
        }
        /// Returns true iff `self` is a local date-time.
        pub fn is_local_date_time(&self) -> bool {
            self.as_local_date_time().is_some()
        }
        /// Returns true iff `self` is a local date.
        pub fn is_local_date(&self) -> bool {
            self.as_local_date().is_some()
        }
        /// Returns true iff `self` is a local time.
        pub fn is_local_time(&self) -> bool {
            self.as_local_time().is_some()
        }
    }
    /// Downcasting
    impl Value {
        /// Casts `self` to integer.
        pub fn as_integer(&self) -> Option<i64> {
            match *self {
                Value::Integer(ref value) => Some(*value.value()),
                _ => None,
            }
        }
        /// Returns true iff `self` is an integer.
        pub fn is_integer(&self) -> bool {
            self.as_integer().is_some()
        }
        /// Casts `self` to float.
        pub fn as_float(&self) -> Option<f64> {
            match *self {
                Value::Float(ref value) => Some(*value.value()),
                _ => None,
            }
        }
        /// Returns true iff `self` is a float.
        pub fn is_float(&self) -> bool {
            self.as_float().is_some()
        }
        /// Casts `self` to boolean.
        pub fn as_bool(&self) -> Option<bool> {
            match *self {
                Value::Boolean(ref value) => Some(*value.value()),
                _ => None,
            }
        }
        /// Returns true iff `self` is a boolean.
        pub fn is_bool(&self) -> bool {
            self.as_bool().is_some()
        }
        /// Casts `self` to str.
        pub fn as_str(&self) -> Option<&str> {
            match *self {
                Value::String(ref value) => Some(value.value()),
                _ => None,
            }
        }
        /// Returns true iff `self` is a string.
        pub fn is_str(&self) -> bool {
            self.as_str().is_some()
        }
        /// Casts `self` to date-time.
        pub fn as_date_time(&self) -> Option<&DateTime> {
            match *self {
                Value::DateTime(ref value) => Some(value.value()),
                _ => None,
            }
        }
        /// Returns true iff `self` is a date-time.
        pub fn is_date_time(&self) -> bool {
            self.as_date_time().is_some()
        }
        /// Casts `self` to array.
        pub fn as_array(&self) -> Option<&Array> {
            match *self {
                Value::Array(ref value) => Some(value),
                _ => None,
            }
        }
        /// Casts `self` to mutable array.
        pub fn as_array_mut(&mut self) -> Option<&mut Array> {
            match *self {
                Value::Array(ref mut value) => Some(value),
                _ => None,
            }
        }
        /// Returns true iff `self` is an array.
        pub fn is_array(&self) -> bool {
            self.as_array().is_some()
        }
        /// Casts `self` to inline table.
        pub fn as_inline_table(&self) -> Option<&InlineTable> {
            match *self {
                Value::InlineTable(ref value) => Some(value),
                _ => None,
            }
        }
        /// Casts `self` to mutable inline table.
        pub fn as_inline_table_mut(&mut self) -> Option<&mut InlineTable> {
            match *self {
                Value::InlineTable(ref mut value) => Some(value),
                _ => None,
            }
        }
        /// Returns true iff `self` is an inline table.
        pub fn is_inline_table(&self) -> bool {
            self.as_inline_table().is_some()
        }
        pub(crate) fn get_type(&self) -> ValueType {
            match *self {
                Value::Integer(..) => ValueType::Integer,
                Value::String(..) => ValueType::String,
                Value::Float(..) => ValueType::Float,
                Value::DateTime(..) => ValueType::DateTime,
                Value::Boolean(..) => ValueType::Boolean,
                Value::Array(..) => ValueType::Array,
                Value::InlineTable(..) => ValueType::InlineTable,
            }
        }
    }
    impl Value {
        /// Get the decoration of the value.
        /// # Example
        /// ```rust
        /// let v = toml_edit::Value::from(true);
        /// assert_eq!(v.decor().suffix(), "");
        ///```
        pub fn decor(&self) -> &Decor {
            match *self {
                Value::Integer(ref f) => &f.repr.decor,
                Value::String(ref f) => &f.repr.decor,
                Value::Float(ref f) => &f.repr.decor,
                Value::DateTime(ref f) => &f.repr.decor,
                Value::Boolean(ref f) => &f.repr.decor,
                Value::Array(ref a) => &a.decor,
                Value::InlineTable(ref t) => &t.decor,
            }
        }
    }
    pub(crate) fn sort_key_value_pairs(items: &mut LinkedHashMap<String, TableKeyValue>) {
        let mut keys: Vec<String> =
            items.iter().filter_map(|i| (i.1).value.as_value().map(|_| i.0)).cloned().collect();
        keys.sort();
        for key in keys {
            items.get_refresh(&key);
        }
    }
    impl FromStr for Value {
        type Err = parser::TomlError;
        /// Parses a value from a &str
        fn from_str(s: &str) -> Result<Self, Self::Err> {
            use combine::Parser;
            let parsed = parser::value_parser().easy_parse(State::new(s));
            match parsed {
                Ok((_, ref rest)) if !rest.input.is_empty() => {
                    Err(Self::Err::from_unparsed(rest.positioner, s))
                }
                Ok((value, _)) => Ok(value),
                Err(e) => Err(Self::Err::new(e, s)),
            }
        }
    }
}
pub use self::{
    array_of_tables::ArrayOfTables,
    document::Document,
    key::Key,
    parser::TomlError,
    table::{array, table, value, Item, Iter, Table, TableLike},
    value::{Array, InlineTable, Value},
};
pub use formatted::decorated;
