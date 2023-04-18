return {
  "mbbill/undotree",
  config = function()
    vim.keymap.set("n", "<localleader>u", ":UndotreeToggle<cr>")
  end,
}
