return {
  "folke/trouble.nvim",
  config = function()
    require("trouble").setup({})
    vim.keymap.set(
      "n",
      "<localleader>t",
      "<cmd>TroubleToggle<cr>",
      { silent = true, noremap = true }
    )
  end,
}
