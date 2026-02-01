use crate::file_set::{File, FileSet};
use crate::noder::Noder;
use crate::parser::Parser;

pub struct Compiler {
    source: String,
}

impl Compiler {
    pub fn new(source: String) -> Self {
        Compiler { source }
    }

    pub fn compile(&self) {
        let file = File::new_from_source("tmp.manta".to_string(), "mod main".to_string());
        let mut fset = FileSet::new(vec![file]);
        let parser = Parser::new();
        let module = parser.parse_module(&mut fset).unwrap();

        let noder = Noder::new();
        let _node_tree = noder.node(&fset, module);
    }
}
