return {
  "phaazon/hop.nvim",
  branch = "v2",
  config = function()
    require("hop").setup({
      keys = "arsdheioqwfpgjluy;zxcvbkmtn",
    })
    vim.keymap.set(
      "n",
      "<leader>/",
      "<cmd>HopChar2<cr>",
      { noremap = true, desc = "Hop to pattern" }
    )

    vim.keymap.set(
      "n",
      "<leader>?",
      "<cmd>HopPattern<cr>",
      { noremap = true, desc = "Hop to pattern" }
    )
  end,
}
