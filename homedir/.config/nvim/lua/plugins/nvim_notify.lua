return {
  "rcarriga/nvim-notify",
  enabled = false,
  init = function()
    vim.opt.termguicolors = true
    require("notify").setup({
      background_colour = "FloatShadow",
    })
    vim.notify = require("notify")
  end,
}
