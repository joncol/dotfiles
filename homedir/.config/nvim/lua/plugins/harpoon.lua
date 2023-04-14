return {
  "ThePrimeagen/harpoon",
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  config = function()
    require("harpoon").setup({
      global_settings = {
        mark_branch = true,
      }
    })
    local mark = require("harpoon.mark")
    local ui = require("harpoon.ui")

    vim.keymap.set("n", "gH", function()
      ui.toggle_quick_menu()
    end)

    vim.keymap.set("n", "gh", function()
      if vim.v.count > 0 then
        ui.nav_file(vim.v.count)
      else
        mark.toggle_file()
      end
    end, { silent = true })
  end,
}
