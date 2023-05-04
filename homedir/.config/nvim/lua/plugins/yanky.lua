return {
  "gbprod/yanky.nvim",
  config = function()
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
    require("yanky").setup()
  end,
}
