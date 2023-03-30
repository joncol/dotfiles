return {
  "zef/vim-cycle",
  config = function()
    vim.keymap.set("n", "+", "<Plug>CycleNext", { desc = "Cycle" })
  end
}
