return {
  "numToStr/FTerm.nvim",

  config = function()
    require("FTerm").setup({ blend = 30 })
    vim.keymap.set("n", "<A-cr>", ':lua require("FTerm").toggle()<cr>')
    vim.keymap.set(
      "t",
      "<A-cr>",
      '<c-\\><c-n>:lua require("FTerm").toggle()<cr>'
    )
  end,
}
