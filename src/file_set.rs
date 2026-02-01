use std::collections::HashMap;
use std::fs;
use std::io;

use crate::parser::lexer::keywords;

pub struct File {
    file_path: String,
    source: String,
    offset: usize,
}

type FileDoesNotExist = io::Error;

impl File {
    pub fn new_from_source(file_path: String, source: String) -> Self {
        File {
            file_path,
            source,
            offset: 0,
        }
    }

    pub fn new_from_path(file_path: String) -> Result<Self, FileDoesNotExist> {
        match fs::read_to_string(&file_path) {
            Ok(source) => Ok(File {
                file_path,
                source,
                offset: 0,
            }),
            Err(e) => return Err(e),
        }
    }
}

/// A type alias for string identifiers. Used to efficiently reference interned strings
/// without storing duplicate string data.
/// StrID types are safe to compare like strings since the same string will always map to the
/// same StrID
pub type StrID = usize;

/// A string interning store that maps strings to unique identifiers.
///
/// This structure allows efficient deduplication of strings during lexing and parsing.
/// Instead of storing the same string multiple times in memory, we store it once and
/// reference it by a small numeric ID.
///
/// # Example
/// ```
/// let mut store = FileSet::new();
/// let id1 = store.get_id("hello");
/// let id2 = store.get_id("hello"); // Returns the same ID without storing "hello" twice
/// assert_eq!(id1, id2);
/// ```
pub struct FileSet<'a> {
    /// All the files in this file set
    files: Vec<File>,
    /// Map from string slices to their unique identifiers
    strings: HashMap<&'a str, StrID>,
    /// Sequential list of strings for reverse lookup (index = StrID)
    reverse_strings: Vec<&'a str>,
    /// Counter for generating the next unique ID
    next_id: usize,
}

impl<'a> FileSet<'a> {
    /// Creates a new empty string store.
    pub fn new(mut files: Vec<File>) -> Self {
        let mut offset = 0;
        for f in files.iter_mut() {
            f.offset = offset;
            offset += f.source.len();
        }

        let mut fset = FileSet {
            files,
            strings: HashMap::new(),
            reverse_strings: Vec::new(),
            next_id: 0,
        };

        // make sure every fileset has all the important keywords
        fset.get_id(keywords::U8);
        fset.get_id(keywords::U16);
        fset.get_id(keywords::U32);
        fset.get_id(keywords::U64);

        fset.get_id(keywords::I8);
        fset.get_id(keywords::I16);
        fset.get_id(keywords::I32);
        fset.get_id(keywords::I64);

        fset.get_id(keywords::F32);
        fset.get_id(keywords::F64);

        fset.get_id(keywords::TRUE);
        fset.get_id(keywords::FALSE);

        fset.get_id(keywords::FN);
        fset.get_id(keywords::WHILE);
        fset.get_id(keywords::FOR);
        fset.get_id(keywords::BREAK);
        fset.get_id(keywords::CONTINUE);
        fset.get_id(keywords::RETURN);
        fset.get_id(keywords::SWITCH);
        fset.get_id(keywords::MATCH);
        fset.get_id(keywords::CONST);
        fset.get_id(keywords::LET);
        fset.get_id(keywords::VAR);
        fset.get_id(keywords::TYPE);
        fset.get_id(keywords::MOD);
        fset.get_id(keywords::MUT);

        fset
    }

    /// files returns a reference to the files in the FileSet
    pub fn files(&self) -> &Vec<File> {
        &self.files
    }

    pub fn substr(&self, start: usize, end: usize) -> Option<&str> {
        // ensure both the start and end are in the same file
        for f in &self.files {
            let f_start = f.offset;
            let f_end = f.offset + f.source.len();

            // start and end are in different files
            if start > f_start && end > f_end {
                break;
            }

            if start > f_start && end < f_end {
                return Some(&f.source[start - f.offset..end - f.offset]);
            }
        }

        return None;
    }

    /// get the StrID for known keywords. These are always included in the FileSet.
    pub fn get_keyword(&self, keyword: keywords::Keyword) -> StrID {
        match self.strings.get(keyword) {
            Some(id) => *id,
            None => panic!("this should never happen"),
        }
    }

    /// Returns or creates an interned ID for the given string.
    ///
    /// If the string has been seen before, returns its existing ID.
    /// Otherwise, assigns a new unique ID to this string and stores it.
    pub fn get_id(&mut self, s: &'a str) -> StrID {
        match self.strings.get(s) {
            // String already interned, return its existing ID
            Some(id) => *id,
            // New string encountered, assign it a new ID
            None => {
                let id = self.next_id;
                self.next_id += 1;
                self.strings.insert(s, id);
                self.reverse_strings.push(s);
                id
            }
        }
    }

    /// Look up the string for a given ID. Returns None if ID not found.
    pub fn get_string(&self, id: StrID) -> Option<&'a str> {
        self.reverse_strings.get(id).copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::lexer::keywords;

    #[test]
    fn test_all_keywords_dont_panic() {
        let store = FileSet::new(vec![]);
        store.get_keyword(keywords::U8);
        store.get_keyword(keywords::U16);
        store.get_keyword(keywords::U32);
        store.get_keyword(keywords::U64);

        store.get_keyword(keywords::I8);
        store.get_keyword(keywords::I16);
        store.get_keyword(keywords::I32);
        store.get_keyword(keywords::I64);

        store.get_keyword(keywords::F32);
        store.get_keyword(keywords::F64);

        store.get_keyword(keywords::TRUE);
        store.get_keyword(keywords::FALSE);

        store.get_keyword(keywords::FN);
        store.get_keyword(keywords::WHILE);
        store.get_keyword(keywords::FOR);
        store.get_keyword(keywords::BREAK);
        store.get_keyword(keywords::CONTINUE);
        store.get_keyword(keywords::RETURN);
        store.get_keyword(keywords::SWITCH);
        store.get_keyword(keywords::MATCH);
        store.get_keyword(keywords::CONST);
        store.get_keyword(keywords::LET);
        store.get_keyword(keywords::VAR);
        store.get_keyword(keywords::TYPE);
        store.get_keyword(keywords::MOD);
        store.get_keyword(keywords::MUT);
    }

    #[test]
    fn test_duplicate_string_returns_same_id() {
        let mut store = FileSet::new(vec![]);
        let id1 = store.get_id("hello");
        store.get_id("other");
        let id2 = store.get_id("hello");
        assert_eq!(id1, id2);
    }

    #[test]
    fn test_different_strings_get_different_ids() {
        let mut store = FileSet::new(vec![]);
        let id1 = store.get_id("hello");
        let id2 = store.get_id("world");
        store.get_id("hello");
        assert_ne!(id1, id2);
    }

    #[test]
    fn test_empty_string_interning() {
        let mut store = FileSet::new(vec![]);
        let id1 = store.get_id("");
        let id2 = store.get_id("");
        assert_eq!(id1, id2);
        assert_eq!(id1, 0);
    }

    #[test]
    fn test_string_with_special_characters() {
        let mut store = FileSet::new(vec![]);
        let id1 = store.get_id("hello\nworld\t!");
        let id2 = store.get_id("hello\nworld\t!");
        assert_eq!(id1, id2);
    }

    #[test]
    fn test_unicode_strings() {
        let mut store = FileSet::new(vec![]);
        let id1 = store.get_id("café");
        let id2 = store.get_id("café");
        let id3 = store.get_id("🦀");
        assert_eq!(id1, id2);
        assert_ne!(id1, id3);
    }

    #[test]
    fn test_long_string_interning() {
        let mut store = FileSet::new(vec![]);
        let long_string = "a".repeat(1000);
        let id1 = store.get_id(&long_string);
        let id2 = store.get_id(&long_string);
        assert_eq!(id1, id2);
    }

    #[test]
    fn test_multiple_different_strings() {
        let mut store = FileSet::new(vec![]);
        let mut ids = Vec::new();
        let strings = vec!["apple", "banana", "cherry", "date", "fig"];

        for s in &strings {
            ids.push(store.get_id(s));
        }

        // All IDs should be unique
        ids.sort();
        for i in 0..ids.len() {
            for j in i + 1..ids.len() {
                assert_ne!(ids[i], ids[j]);
            }
        }

        // Retrieving same strings should return same IDs
        for (i, s) in strings.iter().enumerate() {
            assert_eq!(store.get_id(s), ids[i]);
        }
    }

    #[test]
    fn test_store_size_grows_correctly() {
        let mut store = FileSet::new(vec![]);
        assert_eq!(store.strings.len(), 0);

        store.get_id("first");
        assert_eq!(store.strings.len(), 1);

        store.get_id("second");
        assert_eq!(store.strings.len(), 2);

        store.get_id("first");
        assert_eq!(store.strings.len(), 2);
    }

    #[test]
    fn test_whitespace_strings_are_different() {
        let mut store = FileSet::new(vec![]);
        let id1 = store.get_id("hello");

        // TODO: do we actually want the prefix/suffix whitespace to matter?
        // will this even come up?
        let id2 = store.get_id(" hello");
        let id3 = store.get_id("hello ");
        assert_ne!(id1, id2);
        assert_ne!(id1, id3);
        assert_ne!(id2, id3);
    }
}
