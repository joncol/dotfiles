return {
  "rlch/github-notifications.nvim",

  dependencies = { "nvim-telescope/telescope.nvim" },

  config = function()
    local secrets = require("secrets")

    require("github-notifications").setup({
      username = secrets.github_username,
      token = secrets.github_notifications_token,
      mappings = {
        mark_read = "!",
        hide = "d",
        open_in_browser = '<CR>',
      },
    })

    require("telescope").load_extension("ghn")

    vim.keymap.set("n", "<leader>gn", "<Cmd>Telescope ghn<CR>")
  end,
}
