# Tree-sitter Grammar for Manta

A Tree-sitter grammar for the Manta programming language, enabling syntax highlighting and parsing support in various editors including Neovim.

## Overview

This directory contains the Tree-sitter grammar definition for Manta, a systems programming language with manual memory management and strong type safety. The grammar supports all Manta language constructs including:

- **Module system**: `mod` declarations and `use` imports
- **Type system**: Type aliases, struct types, and enum (sum) types
- **Variables**: Immutable and mutable bindings with type inference
- **Functions**: Function declarations with parameters and return types
- **Control flow**: `if`/`else`, `for`/`loop`, `match`, `return`, `defer`
- **Expressions**: Binary and unary operators with proper precedence
- **Error handling**: Pattern matching with enum variants

## Files

- `grammar.js` - The main Tree-sitter grammar definition
- `package.json` - Node.js project configuration
- `tree-sitter.json` - Tree-sitter configuration (file types, ABI version)
- `src/` - Generated parser files (created by `tree-sitter generate`)

## Building the Grammar

### Prerequisites

- Tree-sitter CLI (v0.20.8+): `brew install tree-sitter`
- Node.js (for running tree-sitter)

### Generate Parser

```bash
cd tree-sitter
tree-sitter generate
```

This creates:
- `src/parser.c` - Generated C parser implementation
- `src/grammar.json` - JSON representation of the grammar
- `src/node-types.json` - Node type definitions
- `src/tree_sitter/` - Header files for C integration

## Grammar Structure

### High-Level Organization

1. **Program Structure**
   - Source file contains module declarations, imports, and top-level declarations

2. **Declarations**
   - Module: `mod <identifier>`
   - Imports: `use ( "module1" "module2" ... )`
   - Types: Type aliases, structs, enums
   - Variables: let, mut, const declarations
   - Functions: Function definitions

3. **Statements**
   - Expression statements
   - Return, defer, if/else, match, for/loop, break, continue

4. **Expressions**
   - Properly precedenced binary operators (arithmetic, logical, comparison)
   - Unary operators (!, -, *, &)
   - Postfix operations (function calls, indexing, field access)
   - Primary expressions (literals, identifiers, constructors)

5. **Types**
   - Primitive types: i8-i64, u8-u64, f32, f64, bool, str, meta
   - Type constructors: pointers (*T), arrays ([N]T, []T)
   - Type aliases and references

## Development

### Modifying the Grammar

Edit `grammar.js` to add or modify grammar rules. Common patterns:

```javascript
// Choice between options
rule: $ => choice(
  $.option1,
  $.option2,
)

// Sequence of rules
rule: $ => seq(
  'keyword',
  $.expression,
  ';',
)

// Repetition
rule: $ => repeat($.item)  // 0 or more
rule: $ => repeat1($.item) // 1 or more

// Optional
rule: $ => optional($.item)

// Precedence and associativity
rule: $ => prec.left(1, seq(...))
rule: $ => prec.right(2, seq(...))
```

### Testing Changes

After modifying `grammar.js`:

```bash
tree-sitter generate  # Rebuild parser
# Use the parser in Neovim or with tree-sitter command
```

## Testing

The grammar has been tested with the example Manta files included in the repository:

- `examples/defer_free.manta` - Tests defer blocks, memory management, and error handling
- `examples/try_catch.manta` - Tests error propagation patterns with enum variants
- `examples/option_match.manta` - Tests match expressions and pattern matching
- `examples/allocate_memory.manta` - Tests memory allocation patterns and type declarations

To verify the grammar works with these examples:

1. Ensure the grammar is built:
```bash
cd tree-sitter
tree-sitter generate
```

2. The grammar correctly parses all example files without errors
3. All language constructs (keywords, types, operators) are properly recognized

### Testing in Neovim

Once installed in Neovim, test the highlighting by opening any `.manta` file:

```bash
nvim examples/try_catch.manta
```

Verify:
- Keywords (`fn`, `let`, `mod`, `use`, `match`, `if`, `for`, `loop`, `defer`) are highlighted
- Types are properly colored
- Enum variants (`.Ok`, `.Err`) are highlighted distinctly
- Comments start with `//` and are properly highlighted
- Strings and numbers are recognized
- Indentation is automatic and correct
- Code folding works (`:set foldmethod=expr`)

## Integration with Neovim

The grammar is used by the Neovim Tree-sitter plugin system. See `../nvim/manta-nvim/` for Neovim-specific configuration including:

- Syntax highlighting queries (`highlights.scm`)
- Indentation rules (`indents.scm`)
- Code folding (`folds.scm`)

## Known Limitations

- Comments must not contain lookahead patterns (tree-sitter limitation)
- Error recovery is basic; malformed code may not parse correctly
- Some advanced features may require query-level workarounds in Neovim

## Future Enhancements

- Support for generics/type parameters
- Better error recovery rules
- Incremental parsing optimizations
- Integration with language server for semantic highlighting

## Resources

- [Tree-sitter Documentation](https://tree-sitter.github.io/tree-sitter/)
- [Manta Language Specification](../docs/language_spec.md)
- [Grammar DSL Reference](https://tree-sitter.github.io/tree-sitter/creating-parsers)
