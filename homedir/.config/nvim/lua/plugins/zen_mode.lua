return {
  "folke/zen-mode.nvim",
  config = function()
    require("zen-mode").setup({
      window = {
        backdrop = 0.8,
        width = 80,
      },
      plugins = {
        tmux = { enabled = true },
      },
    })
    vim.keymap.set("n", "<localleader>z", "<Cmd>ZenMode<CR>")
  end,
}
