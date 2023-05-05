return {
  "ruifm/gitlinker.nvim",

  dependencies = { "nvim-lua/plenary.nvim" },

  config = function()
    require("gitlinker").setup({ mappings = nil })
    vim.keymap.set(
      "n",
      "<leader>gl",
      '<Cmd>lua require"gitlinker".get_buf_range_url("n", {action_callback = require"gitlinker.actions".open_in_browser})<CR>',
      { silent = true }
    )
    vim.keymap.set(
      "x",
      "<leader>gl",
      '<Cmd>lua require"gitlinker".get_buf_range_url("v", {action_callback = require"gitlinker.actions".open_in_browser})<CR>',
      {}
    )
  end,
}
