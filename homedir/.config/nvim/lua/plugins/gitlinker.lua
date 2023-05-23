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
    vim.api.nvim_set_keymap(
      "n",
      "<leader>gY",
      '<Cmd>lua require("gitlinker").get_repo_url()<CR>',
      { silent = true }
    )
    vim.api.nvim_set_keymap(
      "n",
      "<leader>gB",
      '<Cmd>lua require("gitlinker").get_repo_url({ action_callback = require("gitlinker.actions").open_in_browser })<CR>',
      { silent = true }
    )
  end,
}
