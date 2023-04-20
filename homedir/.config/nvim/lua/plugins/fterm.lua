return {
  "numToStr/FTerm.nvim",

  config = function()
    vim.keymap.set("n", "<A-cr>", ':lua require("FTerm").toggle()<cr>')
    vim.keymap.set(
      "t",
      "<A-cr>",
      '<c-\\><c-n>:lua require("FTerm").toggle()<cr>'
    )
  end,
}
