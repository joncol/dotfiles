return {
  "numToStr/Comment.nvim",
  -- enabled = false,
  config = function()
    require("Comment").setup()

    local api = require("Comment.api")
    local esc = vim.api.nvim_replace_termcodes("<esc>", true, false, true)
    local opts = { noremap = true, silent = true }
    vim.keymap.set("n", "<A-;>", "<Plug>(comment_toggle_linewise_current)", opts)
    vim.keymap.set("x", "<A-;>", "<Plug>(comment_toggle_linewise_visual)", opts)
  end
}
