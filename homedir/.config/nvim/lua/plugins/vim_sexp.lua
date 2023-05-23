return {
  { "tpope/vim-sexp-mappings-for-regular-people" },
  {
    "guns/vim-sexp",
    config = function()
      vim.g.sexp_filetypes = vim.g.sexp_filetypes .. ",fennel"
    end,
  },
}
