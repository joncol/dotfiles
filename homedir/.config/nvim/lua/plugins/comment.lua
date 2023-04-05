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

    -- Comment and copy.
    vim.keymap.set(
      "n",
      "<leader>cc",
      "yy<Plug>(comment_toggle_linewise_current)p"
    )
    vim.keymap.set(
      "x",
      "<leader>cc",
      "ygv<Plug>(comment_toggle_linewise_visual)`>p"
    )
  end,
}
