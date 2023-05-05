return {
  "stevearc/aerial.nvim",

  dependencies = { "nvim-telescope/telescope.nvim" },

  config = function()
    require("aerial").setup({
      on_attach = function(bufnr)
        vim.keymap.set("n", "{", "<cmd>AerialPrev<CR>", { buffer = bufnr })
        vim.keymap.set("n", "}", "<cmd>AerialNext<CR>", { buffer = bufnr })
      end,
      filter_kind = {
        "Class",
        "Constructor",
        "Enum",
        "Function",
        "Interface",
        -- "Module",
        "Method",
        "Struct",
      },
    })
    vim.keymap.set("n", "<localleader>a", "<Cmd>AerialToggle!<CR>")

    require("telescope").load_extension("aerial")

    vim.keymap.set(
      "n",
      "<leader>fa",
      ":lua require('telescope').extensions.aerial.aerial()<CR>"
    )
  end,
}
