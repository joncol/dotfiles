return {
  {
    "Olical/aniseed",
  },
  {
    "Olical/conjure",
  },
  {
    "PaterJason/cmp-conjure",

    dependencies = { "hrsh7th/nvim-cmp" },

    config = function()
      require("cmp").setup({
        sources = {
          { name = "conjure" },
        },
      })
    end,
  },
}
