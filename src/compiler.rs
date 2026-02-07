use crate::noder::Noder;
use crate::parser::Parser;
use crate::str_store::StrStore;

pub struct Compiler {
    _source: String,
}

impl Compiler {
    pub fn new(source: String) -> Self {
        Compiler { _source: source }
    }

    pub fn compile(&self) {
        // TODO: this should actually be created in the parse_module call and
        // be owned by the module itself
        let mut str_store = StrStore::new();

        let parser = Parser::new("mod main".to_string());
        let module = parser.parse_module(&mut str_store).unwrap();

        // TODO: if the str_store was part of the module this wouldn't be necessary
        let noder = Noder::new();
        let _node_tree = noder.node(module);
    }
}
