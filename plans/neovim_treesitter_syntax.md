# Neovim Tree-sitter Syntax Highlighting for Manta

## Problem Statement

Manta is a new systems programming language but lacks syntax highlighting support in Neovim. Users must manually highlight `.manta` files or use generic syntax highlighters that don't properly recognize Manta's specific syntax (keywords, types, error handling constructs, etc).

## Solution Approach

Add Tree-sitter grammar support for Manta so Neovim users can get proper syntax highlighting, indentation, and other language features through Tree-sitter integration.

### Implementation Strategy

**Phase 1: Create Tree-sitter Grammar**
- Build a tree-sitter grammar definition (`grammar.js`) that parses Manta syntax
- Define all keywords, primitives, operators, and constructs from the language spec
- Support: modules, functions, types, statements, expressions, error handling, patterns

**Phase 2: Create Neovim Queries and Plugin**
- Write Tree-sitter query files for syntax highlighting (highlights.scm)
- Create queries for indentation, folding, locals (scope tracking)
- Create Neovim plugin directory structure under `/nvim/manta-nvim/`
- Include configuration for language servers (if applicable)

**Phase 3: Testing & Documentation**
- Test with the provided Manta examples
- Document installation instructions for Neovim users
- Document grammar structure for maintainability

## Work Plan

### Phase 1: Tree-sitter Grammar

- [ ] Create `/tree-sitter/` directory structure at repo root
- [ ] Create `tree-sitter/grammar.js` with core grammar rules:
  - Program structure (module, use, declarations)
  - Type declarations (struct, enum)
  - Function declarations with parameters and return types
  - All keywords (fn, let, mut, var, const, defer, match, if, for, loop, return, break, continue, mod, use, type)
  - Primitive types (i8-i64, u8-u64, f32, f64, bool, str, meta)
  - Composite types (*T, [N]T, []T)
  - Expressions and operators
  - Error handling (or, wrap, !)
  - Comments
  - String and number literals
- [ ] Handle operator precedence and associativity
- [ ] Test grammar generation with Tree-sitter CLI

### Phase 2: Neovim Plugin

- [ ] Create `/nvim/manta-nvim/` plugin directory structure:
  ```
  nvim/manta-nvim/
  ├── plugin/
  │   └── manta.lua
  ├── queries/
  │   ├── manta/
  │   │   ├── highlights.scm
  │   │   ├── indents.scm
  │   │   ├── folds.scm
  │   │   └── locals.scm
  └── README.md
  ```
- [ ] Create `highlights.scm` with syntax highlighting rules:
  - Keywords (different colors)
  - Types and type constructors
  - Functions and function calls
  - Comments
  - String/number literals
  - Error handling constructs
  - Built-in operators
- [ ] Create `indents.scm` for automatic indentation
- [ ] Create `folds.scm` for code folding support
- [ ] Create `locals.scm` for scope tracking (optional but useful)
- [ ] Write `plugin/manta.lua` to register the language
- [ ] Create comprehensive README with installation instructions

### Phase 3: Testing & Documentation

- [ ] Test all example `.manta` files for proper highlighting
- [ ] Verify indentation and folding work correctly
- [ ] Create installation guide (Packer, vim-plug, lazy.nvim formats)
- [ ] Document grammar structure for contributors
- [ ] Add to repository README

## Technical Considerations

1. **Tree-sitter Compatibility**: Requires Neovim 0.9+ with Tree-sitter support
2. **Grammar Complexity**: Manta has explicit sum types and defer blocks that need careful pattern matching
3. **Error Recovery**: Consider how grammar handles malformed code (Tree-sitter's error nodes)
4. **Scope Tracking**: The locals.scm queries will help with variable highlighting
5. **Integration Points**:
   - Will work with existing Neovim LSP if a language server is created later
   - Supports vim-treesitter ecosystem plugins

## File Structure to Create

```
manta/
├── tree-sitter/
│   ├── grammar.js          # Main grammar definition
│   ├── package.json        # Node package metadata
│   └── src/
│       └── grammar.json    # Generated (not committed)
├── nvim/
│   └── manta-nvim/
│       ├── plugin/
│       │   └── manta.lua
│       ├── queries/
│       │   └── manta/
│       │       ├── highlights.scm
│       │       ├── indents.scm
│       │       ├── folds.scm
│       │       └── locals.scm
│       ├── README.md
│       └── LICENSE
└── plans/
    └── neovim_treesitter_syntax.md (this file)
```

## Dependencies

- Tree-sitter CLI (for building grammar)
- Neovim with Tree-sitter support (0.9+)
- npm/Node.js (for tree-sitter project)

## Success Criteria

1. ✓ Manta files open in Neovim with syntax highlighting
2. ✓ All keywords, types, and constructs are properly colored
3. ✓ Automatic indentation works correctly
4. ✓ Code folding works for functions and blocks
5. ✓ Example `.manta` files display with proper highlighting
6. ✓ Installation is simple and documented

## Future Enhancements

- Language server support (LSP)
- Semantic highlighting based on scope
- Refactoring queries (rename, etc)
- Integration with other Neovim plugins (telescope, etc)
