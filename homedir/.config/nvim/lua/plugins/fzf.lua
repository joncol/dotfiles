return {
  "ibhagwan/fzf-lua",
  config = function()
    vim.env.FZF_DEFAULT_OPTS = nil
    vim.keymap.set(
      "n",
      "<Leader>zb",
      "<Cmd>lua require('fzf-lua').buffers()<CR>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<Leader>zf",
      "<Cmd>lua require('fzf-lua').files()<CR>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<Leader>zr",
      "<Cmd>lua require('fzf-lua').oldfiles()<CR>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<leader>fi",
      [[<Cmd>lua require('fzf-lua').files({cwd = vim.fn.stdpath("config") .. "/lua/plugins"} )<CR>]],
      { noremap = true, desc = "Find init file" }
    )
    vim.keymap.set(
      "n",
      "<Leader>zq",
      "<Cmd>lua require('fzf-lua').quickfix()<CR>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<Leader>zQ",
      "<Cmd>lua require('fzf-lua').quickfix_stack()<CR>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<Leader>zl",
      "<Cmd>lua require('fzf-lua').loclist()<CR>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<Leader>zl",
      "<Cmd>lua require('fzf-lua').loclist_stack()<CR>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<Leader>zG",
      "<Cmd>lua require('fzf-lua').git_commits()<CR>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<Leader>zg",
      "<Cmd>lua require('fzf-lua').git_bcommits()<CR>",
      { silent = true }
    )
  end,
}
