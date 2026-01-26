-- manta-nvim: Neovim Tree-sitter plugin for Manta language
-- This file registers the Manta language with Neovim's Tree-sitter system

local has_treesitter, ts_configs = pcall(require, "nvim-treesitter.configs")

if not has_treesitter then
  vim.notify("nvim-treesitter not found. Please install it to use manta-nvim.", vim.log.levels.WARN)
  return
end

-- Register .manta files as manta filetype
vim.filetype.add({
  extension = {
    manta = "manta",
  },
})

-- Configure Tree-sitter for Manta
ts_configs.setup({
  ensure_installed = { "manta" },
  highlight = {
    enable = true,
    disable = {},
    additional_vim_regex_highlighting = false,
  },
  indent = {
    enable = true,
    disable = {},
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },
  textobjects = {
    select = {
      enable = true,
      lookahead = true,
      keymaps = {
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
      },
    },
    move = {
      enable = true,
      set_jumps = true,
      goto_next_start = {
        ["]m"] = "@function.outer",
        ["]]"] = "@class.outer",
      },
      goto_previous_start = {
        ["[m"] = "@function.outer",
        ["[["] = "@class.outer",
      },
    },
  },
})

-- Set up syntax highlighting and filetype options
vim.api.nvim_create_autocmd("FileType", {
  pattern = "manta",
  callback = function()
    -- Set up comment string for the language
    vim.bo.commentstring = "// %s"

    -- Configure indentation
    vim.bo.expandtab = true
    vim.bo.shiftwidth = 4
    vim.bo.tabstop = 4
    vim.bo.softtabstop = 4
  end,
})
