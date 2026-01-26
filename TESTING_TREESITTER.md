# Tree-sitter Testing Documentation

This document describes how to test the Manta Tree-sitter grammar and Neovim plugin.

## Grammar Testing

### Prerequisites

1. **Tree-sitter CLI** (v0.20.8+)
   ```bash
   brew install tree-sitter
   ```

2. **Node.js** (for npm dependencies)
   ```bash
   # Already included with tree-sitter on most systems
   ```

### Generate the Grammar

```bash
cd tree-sitter
npm install
npx tree-sitter generate
```

This creates:
- `src/parser.c` - Generated C parser
- `src/grammar.json` - Grammar definition in JSON format
- `src/node-types.json` - Node type definitions

### Test Grammar Parsing

Parse example Manta files to verify the grammar recognizes them:

```bash
cd tree-sitter

# Test individual files
npx tree-sitter parse ../examples/defer_free.manta
npx tree-sitter parse ../examples/try_catch.manta
npx tree-sitter parse ../examples/option_match.manta
npx tree-sitter parse ../examples/allocate_memory.manta

# Or test all at once
for f in ../examples/*.manta; do npx tree-sitter parse "$f"; done
```

The parser should successfully parse all example files and create a syntax tree without critical errors.

## Neovim Plugin Testing

### Installation

1. **Copy the plugin to your Neovim configuration:**
   ```bash
   mkdir -p ~/.config/nvim/pack/manta/start
   cp -r nvim/manta-nvim ~/.config/nvim/pack/manta/start/
   ```

2. **Install the Tree-sitter grammar:**
   Open Neovim and run:
   ```vim
   :TSInstall manta
   ```

3. **Verify installation:**
   ```vim
   :checkhealth nvim_treesitter
   ```

   Should show:
   - `manta` grammar installed
   - Tree-sitter support available
   - No critical errors

### Manual Testing

Open an example Manta file in Neovim:

```bash
nvim examples/try_catch.manta
```

#### Syntax Highlighting Test

Verify these elements are highlighted correctly:

- **Keywords** (blue/red depending on theme):
  - `mod`, `use`, `fn`, `type`, `let`, `mut`, `const`
  - `if`, `else`, `for`, `loop`, `match`
  - `return`, `defer`, `break`, `continue`

- **Types** (green/cyan depending on theme):
  - Primitive types: `i32`, `u64`, `f32`, `bool`, `str`
  - Type constructors: `*T`, `[N]T`, `[]T`

- **Literals**:
  - Numbers: `0`, `42`, `3.14`, `0xFF`
  - Strings: `"hello"`, `"file.txt"`
  - Booleans: `true`, `false`

- **Operators**:
  - Arithmetic: `+`, `-`, `*`, `/`, `%`
  - Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
  - Logical: `&&`, `||`, `!`
  - Assignment: `=`, `+=`, `-=`

- **Special Elements**:
  - Enum variants (e.g., `.Ok`, `.Err`) - highlighted as constants
  - Function names in declarations - highlighted as functions
  - Comments starting with `//`

#### Indentation Test

```vim
# Open a file and enable auto-indent
:set autoindent
:set cindent  " or use treesitter indentation

# Type a function and press Enter - should auto-indent the body
```

Expected behavior:
- Function bodies indent by 4 spaces
- Block contents indent properly
- Continuation lines are indented

#### Code Folding Test

In Neovim with folding enabled:

```vim
:set foldmethod=expr
:set foldexpr=nvim_treesitter#foldexpr()
```

Then:
- `zc` - Close fold under cursor (should fold functions, types, blocks)
- `zo` - Open fold
- `zM` - Close all folds
- `zR` - Open all folds

Verify these can be folded:
- Function declarations
- Type declarations (struct/enum)
- Control structures (if/else, match, for/loop)
- Multi-line statements

#### Text Objects Test

With the plugin active, test these text object keybindings:

- `af` - Select outer function (includes signature and braces)
- `if` - Select inner function (just body)
- `ac` - Select outer class/type
- `ic` - Select inner class/type

Position cursor on a function and press `vaf` to select the entire function.

#### Incremental Selection Test

- `gnn` - Initialize selection
- `grn` - Extend selection (node incremental)
- `grc` - Extend selection to scope
- `grm` - Shrink selection

Navigate through a function and use these keybindings to verify selection works correctly.

### Automated Testing Checklist

Use this checklist when making changes to the grammar or queries:

```
Grammar Tests:
- [ ] All example files parse without critical errors
- [ ] Grammar regenerates without warnings
- [ ] Parser C code compiles (if building from source)

Syntax Highlighting:
- [ ] Keywords are highlighted
- [ ] Type names are highlighted
- [ ] Comments are recognized
- [ ] String/number literals are highlighted
- [ ] Operators are highlighted
- [ ] Enum variants display as constants

Indentation:
- [ ] Functions auto-indent body
- [ ] Blocks auto-indent contents
- [ ] Nested structures indent correctly
- [ ] Continuation lines indent properly

Code Folding:
- [ ] Functions can be folded
- [ ] Type declarations can be folded
- [ ] Control structures can be folded
- [ ] Multi-line statements fold correctly

Scope Tracking (locals):
- [ ] Function names are recognized
- [ ] Variable names are recognized
- [ ] Parameter names are recognized
- [ ] Type names are recognized
```

## Testing Different Color Schemes

Test the highlighting with different Neovim color schemes to ensure the capture groups work well with various themes:

```vim
:colorscheme tokyonight
" Test highlighting
:colorscheme gruvbox
" Test highlighting
:colorscheme nord
" Test highlighting
```

Highlight groups should adapt to the color scheme while maintaining good readability.

## Troubleshooting

### Syntax highlighting not working

1. Verify the plugin is installed:
   ```bash
   ls ~/.config/nvim/pack/manta/start/manta-nvim/
   ```

2. Check that highlighting is enabled:
   ```vim
   :set highlight=@capture_group
   :TSBufEnable highlight
   ```

3. Verify the grammar is installed:
   ```vim
   :TSInstall manta
   :checkhealth nvim_treesitter
   ```

### Indentation not working

1. Enable tree-sitter indentation:
   ```vim
   :set indentexpr=nvim_treesitter#indent()
   ```

2. Or configure in init.lua:
   ```lua
   require("nvim-treesitter.configs").setup({
     indent = { enable = true }
   })
   ```

### Errors in parsing

1. Check the tree-sitter logs:
   ```vim
   :TSEditQuery highlights
   ```

2. Verify the grammar supports the syntax you're testing
3. Check [Grammar Structure](tree-sitter/README.md#grammar-structure) in tree-sitter/README.md

## Performance Testing

Monitor Neovim's responsiveness with Tree-sitter enabled:

1. Open a large Manta file (or create one)
2. Navigate using arrow keys, Page Up/Down
3. Monitor CPU usage: `top` or Activity Monitor
4. Check Neovim response time - should be responsive
5. Verify no noticeable lag when scrolling or editing

## Contributing Test Results

When reporting issues or submitting improvements:

1. Run the full testing checklist above
2. Include the version of Neovim: `:version`
3. Include Tree-sitter version: `nvim --version | grep treesitter`
4. Provide example code that demonstrates the issue
5. Describe what you expected vs what actually happened

## Example Test Run

```bash
# 1. Build grammar
cd tree-sitter
npm install
npx tree-sitter generate
# ✓ Grammar generated successfully

# 2. Parse example files
for f in ../examples/*.manta; do
  echo "Testing $(basename $f)..."
  npx tree-sitter parse "$f" | head -1  # Should output (source_file [...])
done
# ✓ All files parse successfully

# 3. Open in Neovim and verify visually
nvim ../examples/try_catch.manta
# ✓ Syntax highlighting works
# ✓ Indentation is correct
# ✓ Code folding works
```

## Resources

- [Tree-sitter Documentation](https://tree-sitter.github.io/tree-sitter/)
- [Neovim Tree-sitter Documentation](https://neovim.io/doc/user/treesitter.html)
- [nvim-treesitter GitHub](https://github.com/nvim-treesitter/nvim-treesitter)
- [manta-nvim README](nvim/manta-nvim/README.md)
- [Tree-sitter Grammar README](tree-sitter/README.md)
