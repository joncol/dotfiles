return {
  -- "simrat39/symbols-outline.nvim",

  -- See: https://github.com/simrat39/symbols-outline.nvim/issues/176
  "enddeadroyal/symbols-outline.nvim",
  branch = "bugfix/symbol-hover-misplacement",

  config = function()
    require("symbols-outline").setup()
    vim.keymap.set("n", "<localleader>s", "<Cmd>SymbolsOutline<CR>")
  end,
}
