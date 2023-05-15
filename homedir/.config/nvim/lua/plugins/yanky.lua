return {
  "gbprod/yanky.nvim",

  dependencies = { "nvim-telescope/telescope.nvim" },

  config = function()
    local actions = require("telescope.actions")
    local utils = require("yanky.utils")
    local mapping = require("yanky.telescope.mapping")

    require("yanky").setup({
      picker = {
        telescope = {
          default = mapping.put("p"),
          mappings = {
            i = {
              ["<C-n>"] = actions.move_selection_next,
              ["<C-p>"] = actions.move_selection_previous,
              ["<C-x>"] = mapping.delete(),
              ["<C-r>"] = mapping.set_register(utils.get_default_register()),
              ["<A-CR>"] = mapping.put("P"),
            },
            n = {
              p = mapping.put("p"),
              P = mapping.put("P"),
              d = mapping.delete(),
              r = mapping.set_register(utils.get_default_register()),
            },
          },
        },
      },
    })

    vim.keymap.set({ "n", "x" }, "p", "<Plug>(YankyPutAfter)")
    vim.keymap.set({ "n", "x" }, "P", "<Plug>(YankyPutBefore)")
    vim.keymap.set({ "n", "x" }, "gp", "<Plug>(YankyGPutAfter)")
    vim.keymap.set({ "n", "x" }, "gP", "<Plug>(YankyGPutBefore)")

    local keymap = vim.keymap
    local yanky = require("yanky")
    keymap.amend = require("keymap-amend")

    keymap.amend("n", "<C-p>", function(f)
      if yanky.can_cycle() then
        yanky.cycle(1)
      else
        f()
      end
    end)

    keymap.amend("n", "<C-n>", function(f)
      if yanky.can_cycle() then
        yanky.cycle(-1)
      else
        f()
      end
    end)

    require("telescope").load_extension("yank_history")

    vim.keymap.set("n", "<A-y>", "<Cmd>Telescope yank_history<CR>")
    vim.keymap.set("i", "<A-y>", "<Cmd>Telescope yank_history<CR>")
  end,
}
