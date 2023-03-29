return {
  "terrortylor/nvim-comment",
  config = function()
    require('nvim_comment').setup({})

    local opts = { noremap = true, silent = true }
    vim.keymap.set("n", "<A-;>", "<cmd>CommentToggle<cr>", opts)
    vim.keymap.set("v", "<A-;>", "<esc><cmd>'<,'>CommentToggle<cr>", opts)
  end
}
