return {
  "junegunn/fzf.vim",
  dependencies = { "junegunn/fzf" },
  config = function()
    vim.env.FZF_DEFAULT_OPTS = nil
  end,
}
