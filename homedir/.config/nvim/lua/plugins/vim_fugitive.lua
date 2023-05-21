return {
  "tpope/vim-fugitive",
  config = function()
    vim.keymap.set("n", "<Leader>gg", ":Git<cr>")
    vim.keymap.set("n", "<Leader>gb", ":Git blame<cr>")
  end,
}
