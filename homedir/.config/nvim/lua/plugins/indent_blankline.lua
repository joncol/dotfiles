return {
  "lukas-reineke/indent-blankline.nvim",
  main = "ibl",
  event = "VeryLazy",
  config = function()
    vim.opt.list = true
    vim.opt.listchars = ""
    -- vim.opt.listchars:append("space:⋅")
    -- vim.opt.listchars:append("trail:⋅")
    vim.opt.listchars:append("tab:  ")
    -- vim.opt.listchars:append("eol:↴")

    require("ibl").setup({})
  end,
}
