return {
  "tpope/vim-fugitive",
  config = function()
    vim.keymap.set("n", "<localleader>g", ":Git<cr>", { noremap = true })
  end,
}
