return {
  "tpope/vim-fugitive",
  config = function()
    vim.keymap.set("n", "<localleader>g", ":Git<cr>")
    vim.keymap.set("n", "<localleader>gb", ":Git blame<cr>")
  end,
}
