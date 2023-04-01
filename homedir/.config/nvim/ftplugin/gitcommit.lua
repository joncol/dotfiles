vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter" }, {
  pattern = "COMMIT_EDITMSG",
  callback = function()
    vim.cmd(":startinsert!")
  end,
})
