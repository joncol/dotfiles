return {
  "akinsho/bufferline.nvim",
  enabled = false,
  config = function()
    vim.opt.termguicolors = true
    require("bufferline").setup({})
  end,
}
