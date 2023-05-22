return {
  "tpope/vim-fugitive",
  config = function()
    vim.keymap.set("n", "<Leader>gb", ":Git blame<cr>")
  end,
}
