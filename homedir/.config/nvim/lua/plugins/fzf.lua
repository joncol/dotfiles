return {
  "ibhagwan/fzf-lua",
  config = function()
    vim.env.FZF_DEFAULT_OPTS = nil
    vim.keymap.set(
      "n",
      "<leader>zb",
      "<cmd>lua require('fzf-lua').buffers()<cr>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<leader>zf",
      "<cmd>lua require('fzf-lua').files()<cr>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<leader>zr",
      "<cmd>lua require('fzf-lua').oldfiles()<cr>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<leader>zq",
      "<cmd>lua require('fzf-lua').quickfix()<cr>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<leader>zQ",
      "<cmd>lua require('fzf-lua').quickfix_stack()<cr>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<leader>zl",
      "<cmd>lua require('fzf-lua').loclist()<cr>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<leader>zl",
      "<cmd>lua require('fzf-lua').loclist_stack()<cr>",
      { silent = true }
    )
  end,
}
