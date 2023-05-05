return {
  "simrat39/symbols-outline.nvim",
  config = function ()
    require("symbols-outline").setup()
    vim.keymap.set("n", "<localleader>s", "<Cmd>SymbolsOutline<CR>")
  end
}
