return {
  "monaqa/dial.nvim",

  config = function()
    local augend = require("dial.augend")

    require("dial.config").augends:register_group({
      default = {
        augend.integer.alias.decimal,
        augend.integer.alias.hex,
        augend.date.alias["%m/%d"],
        augend.constant.alias.alpha,
        augend.constant.alias.bool,
      },
      haskell = {
        augend.constant.new({ elements = { "True", "False" } }),
      },
    })

    vim.keymap.set(
      "n",
      "<C-a>",
      require("dial.map").inc_normal(),
      { noremap = true }
    )
    vim.keymap.set(
      "n",
      "+",
      require("dial.map").inc_normal(),
      { noremap = true }
    )
    vim.keymap.set(
      "n",
      "<C-x>",
      require("dial.map").dec_normal(),
      { noremap = true }
    )
    vim.keymap.set(
      "n",
      "g<C-a>",
      require("dial.map").inc_gnormal(),
      { noremap = true }
    )
    vim.keymap.set(
      "n",
      "g<C-x>",
      require("dial.map").dec_gnormal(),
      { noremap = true }
    )
    vim.keymap.set(
      "v",
      "<C-a>",
      require("dial.map").inc_visual(),
      { noremap = true }
    )
    vim.keymap.set(
      "v",
      "<C-x>",
      require("dial.map").dec_visual(),
      { noremap = true }
    )
    vim.keymap.set(
      "v",
      "g<C-a>",
      require("dial.map").inc_gvisual(),
      { noremap = true }
    )
    vim.keymap.set(
      "v",
      "g<C-x>",
      require("dial.map").dec_gvisual(),
      { noremap = true }
    )

    vim.api.nvim_create_autocmd("FileType", {
      pattern = "haskell",
      callback = function()
        vim.api.nvim_buf_set_keymap(
          0,
          "n",
          "<C-a>",
          require("dial.map").inc_normal("haskell"),
          { noremap = true }
        )
        vim.api.nvim_buf_set_keymap(
          0,
          "n",
          "+",
          require("dial.map").inc_normal("haskell"),
          { noremap = true }
        )
        vim.api.nvim_buf_set_keymap(
          0,
          "n",
          "<C-x>",
          require("dial.map").dec_normal("haskell"),
          { noremap = true }
        )
      end,
    })
  end,
}
