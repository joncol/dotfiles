return {
  {
    "nvim-treesitter/nvim-treesitter",

    config = function()
      require("nvim-treesitter").setup({
        ensure_installed = {
          "bash",
          "haskell",
          "html",
          "javascript",
          "json",
          "lua",
          "markdown",
          "markdown_inline",
          "python",
          "query",
          "regex",
          "tsx",
          "typescript",
          "vim",
          "yaml",
        },
      })
      vim.cmd("TSUpdate")
      require("nvim-treesitter.configs").setup({
        ensure_installed = { "c", "lua", "vim", "haskell", "query" },
        sync_install = false,
        auto_install = true,
        ignore_install = {},
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = false,
        },
      })
    end,
  },

  {
    "nvim-treesitter/nvim-treesitter-context",

    config = function()
      require("treesitter-context").setup()
    end,
  },
}
