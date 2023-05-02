return {
  "TheBlob42/houdini.nvim",

  branch = "fix_i_ctrl_a",

  config = function()
    require("houdini").setup({
      mappings = { "lh" },
      timeout = vim.o.timeoutlen,
      check_modified = true,
    })
  end,
}
