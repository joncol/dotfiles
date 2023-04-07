return {
  "hrsh7th/nvim-cmp",
  event = "InsertEnter",
  dependencies = {
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-buffer",
  },
  opts = function(_, opts)
    local cmp = require("cmp")

    opts.snippet = {
      expand = function(args)
        require("luasnip").lsp_expand(args.body)
      end,
    }

    opts.mapping = cmp.mapping.preset.insert({
      ["<C-space>"] = cmp.mapping.complete(),
      ["<tab>"] = cmp.mapping.confirm({ select = true }),
      ["<cr>"] = cmp.mapping.confirm({ select = true }),
    })

    opts.sources = cmp.config.sources({
      { name = "nvim_lsp" },
      { name = "luasnip" },
    }, { name = "buffer" })
  end,
}
