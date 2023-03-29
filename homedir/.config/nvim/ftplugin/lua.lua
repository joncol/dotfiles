vim.opt_local.expandtab = true
vim.opt_local.softtabstop = 2
vim.opt_local.shiftwidth = 2

require("lspconfig").lua_ls.setup({
  settings = {
    Lua = {
      diagnostics = {
        globals = { 'vim' }
      }
    }
  }
})
