return {
  "TheBlob42/houdini.nvim",

  config = function()
    require("houdini").setup({
      mappings = { "lh" },
      timeout = vim.o.timeoutlen,
      check_modified = true,
    })
  end,
}
