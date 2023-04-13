return {
  "folke/trouble.nvim",
  config = function()
    vim.keymap.set(
      "n",
      "<localleader>tt",
      "<cmd>TroubleToggle<cr>",
      { silent = true, noremap = true }
    )

    vim.keymap.set("n", "<localleader>tn", function()
      require("trouble").next({ skip_groups = true, jump = true })
    end, { silent = true, noremap = true })

    vim.keymap.set("n", "<localleader>tp", function()
      require("trouble").previous({ skip_groups = true, jump = true })
    end, { silent = true, noremap = true })

    vim.keymap.set(
      "n",
      "<localleader>tq",
      "<cmd>TroubleToggle quickfix<cr>",
      { silent = true, noremap = true }
    )
    vim.keymap.set(
      "n",
      "<localleader>tl",
      "<cmd>TroubleToggle loclist<cr>",
      { silent = true, noremap = true }
    )

    vim.keymap.set(
      "n",
      "<localleader>tw",
      "<cmd>TroubleToggle workspace_diagnostics<cr>",
      { silent = true, noremap = true }
    )
    vim.keymap.set(
      "n",
      "<localleader>td",
      "<cmd>TroubleToggle document_diagnostics<cr>",
      { silent = true, noremap = true }
    )
  end,
}
