return {
  "numToStr/Comment.nvim",
  config = function()
    require("Comment").setup()

    local opts = { noremap = true, silent = true }
    vim.keymap.set(
      "n",
      "<A-;>",
      "<Plug>(comment_toggle_linewise_current)",
      opts
    )
    vim.keymap.set("x", "<A-;>", "<Plug>(comment_toggle_linewise_visual)", opts)
  end,
}
