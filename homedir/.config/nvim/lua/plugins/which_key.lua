return {
  "folke/which-key.nvim",
  enabled = false,
  config = function()
    vim.o.timeout = true
    vim.o.timeoutlen = 300
    require("which-key").setup({})
  end,
}
