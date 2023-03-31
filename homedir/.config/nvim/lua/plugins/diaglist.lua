return {
  "onsails/diaglist.nvim",

  config = function()
    require("diaglist").init({
      debug = false,
      debounce_ms = 150,
    })
    local diaglist = require("diaglist")
    vim.keymap.set(
      "n",
      "<leader>dd",
      diaglist.open_buffer_diagnostics,
      { desc = "Buffer diagnostics" }
    )
    vim.keymap.set(
      "n",
      "<leader>da",
      diaglist.open_all_diagnostics,
      { desc = "All diagnostics" }
    )
  end,
}
