return {
  {
    "guns/vim-sexp",
    init = function()
      vim.g.sexp_filetypes = "clojure,scheme,lisp,timl,fennel"
    end,
  },
  { "tpope/vim-sexp-mappings-for-regular-people" },
}
