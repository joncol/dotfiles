vim.api.nvim_buf_set_keymap(0, "n", "<C-n>", "<Down>", {})
vim.api.nvim_buf_set_keymap(0, "n", "<C-p>", "<Up>", {})
vim.api.nvim_buf_set_keymap(
  0,
  "n",
  "<C-c>",
  "<Cmd>lua require('code_action_menu').close_code_action_menu()<CR>",
  {}
)
