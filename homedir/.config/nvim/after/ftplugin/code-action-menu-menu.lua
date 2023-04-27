x = 123
vim.keymap.set("n", "<C-n>", "<Down>")
vim.keymap.set("n", "<C-p>", "<Up>")
vim.keymap.set(
  "n",
  "<C-c>",
  "<Cmd>lua require('code_action_menu').close_code_action_menu()<CR>"
)
