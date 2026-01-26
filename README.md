# Manta — systems language compiler (work-in-progress)

Manta is a small, systems-style programming language and compiler project implemented in Rust.
This README is a living document and reflects the compiler's current goals and how to build and run the repository locally.

## Project goals

- Minimal, explicit systems-language with manual memory management (no GC).
- Familiar surface syntax for Rust/C/Zig/Go users: `fn`, `let`, `defer`, `new`/`free`, `match`, `try`/`catch`.
- Small, practical grammar designed to make implementing a lexer, parser, and simple codegen straightforward.

See `docs/language_spec.md` for the detailed language design notes and rationale.

## Building

This project is a Rust crate. To build the compiler locally you need a Rust toolchain (rustc + cargo). On macOS using Homebrew or rustup is recommended.

Build the project:

```bash
cargo build --release
```

Or for a fast debug build:

```bash
cargo build
```

Currently the compiler front-end is an early prototype; `src/main.rs` contains a placeholder entrypoint.

## Running / examples

There are example Manta programs in the `examples/` folder that demonstrate core features:

- `examples/defer_free.manta` — shows `defer`, `new`, `free`, and error handling via `try`/`catch`.
- `examples/nil_refs.manta` — shows `nil` references, conditional allocation, and manual `free`.
- `examples/option_match.manta` — demonstrates `enum` (sum) types and `match` pattern matching.
- `examples/try_catch.manta` — demonstrates `try`/`catch` for error propagation and `defer` usage.

The compiler currently doesn’t yet implement a full CLI to compile and run Manta programs. When a CLI is available the README will be updated with usage examples such as:

```bash
# Build the compiler
cargo build --release

# Compile a manta source file (example placeholder)
./target/release/mantac examples/try_catch.manta -o out.bin

# (or) run with an interpreter backend:
./target/release/mantac run examples/try_catch.manta
```

Note: the above commands are illustrative; please check `src/main.rs` and the CLI help for the current, accurate flags.

## Editor Support

### Neovim with Tree-sitter

Manta has full Tree-sitter syntax highlighting, indentation, and code folding support for Neovim (0.9+).

**Features:**
- ✨ Syntax highlighting for all Manta constructs (keywords, types, operators, literals)
- 📐 Automatic indentation for functions, blocks, and control structures
- 🔖 Code folding for functions, types, and control structures
- 🔍 Scope tracking and variable/function awareness
- ⌨️ Text objects and incremental selection

**Quick Setup:**

1. **Copy the plugin to your Neovim config:**
```bash
mkdir -p ~/.config/nvim/pack/manta/start
cp -r nvim/manta-nvim ~/.config/nvim/pack/manta/start/
```

2. **Install the Tree-sitter grammar in Neovim:**
```vim
:TSInstall manta
```

3. **Verify installation:**
```vim
:checkhealth nvim_treesitter
```

For detailed installation instructions including lazy.nvim, packer.nvim, and vim-plug setup, see the [manta-nvim plugin README](nvim/manta-nvim/README.md).

### Grammar Documentation

The Tree-sitter grammar for Manta is maintained in the `tree-sitter/` directory. It supports all language constructs including modules, type declarations (structs and enums), functions, control flow, and expressions.

See [tree-sitter/README.md](tree-sitter/README.md) for:
- Grammar structure and design
- How to rebuild the grammar
- Development guidelines for extending the grammar

### Testing

For comprehensive information on testing the Tree-sitter grammar and Neovim plugin, see [TESTING_TREESITTER.md](TESTING_TREESITTER.md).

## Project Structure

`parser.rs` and `src/parser` contains the source code lexer and parser code.
`ast.rs` contains the expression nodes and statment nodes.

## Language spec references

Primary design documents are in `docs/`. Key files to read:

- `docs/language_spec.md` — high-level language features, examples, and semantics.
- `docs/grammar.ebnf` — starting grammar for the parser (EBNF draft).
- `docs/token_list.md` — the token kinds the lexer should emit.

These documents were used to author the example programs in `examples/` and should be considered the source-of-truth while the language evolves.

## Command-line interface

The main way to interact with Manta is uing the Manta CLI.
The CLI executable is named `manta` and supports the following subcommands and flags:

### Build
`manta build` Compiles the project and produce build artifacts.
The following flags are supported:

- `-o, --out-dir <OUT_DIR>` : optional output directory for build artifacts

### Check
`manta check` Runs checks on the project and report results without producing build artifacts.

### Run
`manta run` Builds and executes the program immediately.

### Fmt
`manta fmt` Formats source files using the official manta formatter.
The following falgs are supported:

- `-w, --write` : write changes to files in-place (otherwise prints diffs)
- positional `FILES...` : files or directories to format

Global flags:

- `-v` / `--verbose` : increase verbosity (stackable; `-vv` for more verbose output)
- `-p, --path <PATH>` : path to the project or file to operate on (defaults to current directory)

Examples (using the built binary in `target/release`):

```bash
# Build the current project (stub)
./target/release/manta build -o out/ 

# Check project in strict mode
./target/release/manta check

# Run the project, passing program arguments
./target/release/manta run

# Format manta source files in-place
./target/release/manta fmt -w src/
```

# Code Coverage

This project uses `cargo-llvm-cov` for code coverage reporting.

## Installation

```bash
cargo install cargo-llvm-cov
```

## Generating Coverage Reports

Generate an HTML coverage report:
```bash
cargo cov-html
```

View coverage summary in the terminal:
```bash
cargo coc
```

Generate a JSON report:
```bash
cargo cov-json
```
