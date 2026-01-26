# manta-nvim: Neovim Tree-sitter Plugin for Manta

Neovim syntax highlighting, indentation, and code folding support for the [Manta programming language](https://github.com/alexis/manta) using Tree-sitter.

## Features

✨ **Syntax Highlighting**
- Keyword highlighting (fn, let, mut, const, if, match, defer, etc.)
- Type highlighting with built-in type recognition
- Operator and punctuation highlighting
- Enum variant and module-qualified name highlighting
- String and number literal support

📐 **Indentation**
- Automatic indentation for blocks, functions, and control structures
- Smart continuation line indentation
- Supports multi-line statements

🔖 **Code Folding**
- Fold function declarations
- Fold type declarations (structs, enums)
- Fold control structures (if/else, match, for, loop)
- Fold multi-line statements

🔍 **Scope Tracking**
- Variable and function scope awareness
- Type reference tracking
- Parameter and binding recognition

## Requirements

- Neovim 0.9+ (with Tree-sitter support)
- `nvim-treesitter` plugin installed

## Installation

### Using lazy.nvim (Recommended)

Add to your `init.lua`:

```lua
{
  "nvim-treesitter/nvim-treesitter",
  run = ":TSUpdate",
  config = function()
    require("nvim-treesitter.configs").setup({
      ensure_installed = { "manta" },
      highlight = { enable = true },
      indent = { enable = true },
    })
  end,
}
```

Then copy this plugin directory to your Neovim config:

```bash
mkdir -p ~/.config/nvim/pack/manta/start
cp -r path/to/manta-nvim ~/.config/nvim/pack/manta/start/
```

### Using packer.nvim

In your `init.lua`:

```lua
use {
  "nvim-treesitter/nvim-treesitter",
  run = ":TSUpdate",
}

use "path/to/manta-nvim"
```

Then run `:PackerSync` and `:TSInstall manta`.

### Using vim-plug

In your `init.vim`:

```vim
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'path/to/manta-nvim'
```

Then run `:PlugInstall` and `:TSInstall manta`.

## Manual Installation

1. **Copy the plugin directory:**
   ```bash
   mkdir -p ~/.config/nvim/pack/manta/start
   cp -r manta-nvim ~/.config/nvim/pack/manta/start/
   ```

2. **Ensure the Tree-sitter grammar is installed:**
   ```bash
   # The grammar will be automatically installed via nvim-treesitter
   :TSInstall manta
   ```

## Files Structure

```
manta-nvim/
├── plugin/
│   └── manta.lua          # Plugin initialization and configuration
├── queries/
│   └── manta/
│       ├── highlights.scm # Syntax highlighting queries
│       ├── indents.scm    # Indentation rules
│       ├── folds.scm      # Code folding rules
│       └── locals.scm     # Scope and reference tracking
└── README.md              # This file
```

## Configuration

### Default Settings

The plugin sets the following defaults for `.manta` files:

- Comment string: `// %s` (for comment operators)
- Indentation: 4 spaces
- Tab expansion: enabled

### Customizing Highlights

To customize the syntax highlighting, add color scheme definitions for the Manta highlight groups in your Neovim config:

```lua
-- example with tokyo-night theme
local colors = require("tokyonight.colors").setup()

vim.api.nvim_set_hl(0, "@keyword", { fg = colors.blue })
vim.api.nvim_set_hl(0, "@type", { fg = colors.green })
vim.api.nvim_set_hl(0, "@function", { fg = colors.purple })
vim.api.nvim_set_hl(0, "@constant", { fg = colors.orange })
```

### Disabling Features

To disable specific features for Manta files:

```lua
require("nvim-treesitter.configs").setup({
  highlight = {
    enable = true,
    disable = { "manta" },  -- Disable highlighting
  },
  indent = {
    enable = true,
    disable = { "manta" },  -- Disable indentation
  },
})
```

## Tree-sitter Grammar

The Manta Tree-sitter grammar is located in the `tree-sitter/` directory of the main Manta repository.

### Building the Grammar

If you need to rebuild the grammar:

```bash
cd tree-sitter
tree-sitter generate
```

See `../tree-sitter/README.md` for more information.

## Supported Language Features

The plugin fully supports all Manta language constructs:

- **Declarations**: `mod`, `use`, `fn`, `type`, `let`, `mut`, `const`
- **Types**: Primitive types, pointers, arrays, custom types
- **Control Flow**: `if`/`else`, `for`, `loop`, `match`, `return`, `defer`, `break`, `continue`
- **Expressions**: All operators, function calls, field access, indexing
- **Patterns**: Enum variants, literals, bindings, wildcards
- **Error Handling**: Pattern matching with enum variants

## Keybindings

The plugin includes default keybindings for incremental selection (requires `nvim-treesitter`):

- `gnn` - Initialize selection
- `grn` - Node incremental (extend selection)
- `grc` - Scope incremental
- `grm` - Node decremental (shrink selection)

And for text objects:

- `af`/`if` - Select outer/inner function
- `ac`/`ic` - Select outer/inner class/type

These are enabled by default if `nvim-treesitter` is configured.

## Troubleshooting

### "nvim-treesitter not found"

Ensure you have `nvim-treesitter` installed:

```bash
# Using lazy.nvim, packer, or vim-plug, install nvim-treesitter first
```

### Syntax highlighting not working

1. Check that Manta highlighting is enabled:
   ```lua
   require("nvim-treesitter.configs").setup({
     highlight = { enable = true }
   })
   ```

2. Verify the Tree-sitter grammar is installed:
   ```vim
   :TSInstall manta
   :checkhealth nvim_treesitter
   ```

3. Ensure your colorscheme supports Tree-sitter highlighting

### Indentation not working

Make sure indentation is enabled in your Tree-sitter config:

```lua
require("nvim-treesitter.configs").setup({
  indent = { enable = true }
})
```

## Development

### Adding New Highlight Groups

Edit `queries/manta/highlights.scm` and add new capture patterns:

```scm
(some_node) @highlight_group
```

Available highlight groups: `@keyword`, `@type`, `@function`, `@variable`, `@comment`, `@string`, `@number`, `@operator`, `@punctuation`, etc.

### Testing Changes

After modifying query files:

1. Reload Neovim or run `:edit` to reload the current file
2. Check `:checkhealth nvim_treesitter` for any errors

## Contributing

Contributions are welcome! Please:

1. Test changes with the example Manta files
2. Update query files for new language features
3. Document any new behavior

## License

This plugin is part of the Manta project. See the main repository for license information.

## Related

- [Manta Language](https://github.com/alexis/manta)
- [Tree-sitter](https://tree-sitter.github.io/)
- [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter)
