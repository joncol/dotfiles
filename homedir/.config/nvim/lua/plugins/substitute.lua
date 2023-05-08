return {
  "gbprod/substitute.nvim",

  config = function()
    require("substitute").setup({
      range = {
        prefix = "S",
      },
    })
    vim.keymap.set("n", "S", require("substitute").operator, { noremap = true })
    vim.keymap.set("n", "Sx", require("substitute").line, { noremap = true })
    vim.keymap.set("n", "S$", require("substitute").eol, { noremap = true })
    vim.keymap.set("x", "x", require("substitute").visual, { noremap = true })

    vim.keymap.set(
      "n",
      "cx",
      require("substitute.exchange").operator,
      { noremap = true }
    )
    vim.keymap.set(
      "n",
      "cxx",
      require("substitute.exchange").line,
      { noremap = true }
    )
    vim.keymap.set(
      "x",
      "cx",
      require("substitute.exchange").visual,
      { noremap = true }
    )
    vim.keymap.set(
      "n",
      "cxc",
      require("substitute.exchange").cancel,
      { noremap = true }
    )
  end,
}
