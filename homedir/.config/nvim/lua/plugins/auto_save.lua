return {
  "okuuva/auto-save.nvim",
  enabled = false,
  config = function()
    require("auto-save").setup({
      trigger_events = { "BufLeave" },
    })

    vim.api.nvim_set_keymap("n", "<localleader>as", ":ASToggle<cr>", {})
  end,
}
