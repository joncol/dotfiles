return {
  "folke/zen-mode.nvim",
  config = function()
    require("zen-mode").setup({
      window = {
        backdrop = 0.8,
      },
    })
    vim.keymap.set("n", "<localleader>z", "<Cmd>ZenMode<CR>")
  end,
}
