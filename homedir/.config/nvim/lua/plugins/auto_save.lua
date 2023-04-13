return {
  "okuuva/auto-save.nvim",
  config = function()
    require("auto-save").setup({
      trigger_events = { "BufLeave", "FocusLost" },
    })

    vim.api.nvim_set_keymap("n", "<localleader>as", ":ASToggle<cr>", {})
  end,
}
