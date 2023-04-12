return {
  "hrsh7th/nvim-cmp",
  event = "InsertEnter",
  dependencies = {
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-buffer",
  },
  opts = function(_, opts)
    local cmp = require("cmp")
    local luasnip = require("luasnip")

    opts.snippet = {
      expand = function(args)
        luasnip.lsp_expand(args.body)
      end,
    }

    local has_words_before = function()
      local cursor = vim.api.nvim_win_get_cursor(0)
      return (
        vim.api.nvim_buf_get_lines(0, cursor[1] - 1, cursor[1], true)[1] or ""
      ):sub(cursor[2], cursor[2]):match("%s")
    end

    opts.mapping = cmp.mapping.preset.insert({
      ["<C-space>"] = cmp.mapping.complete(),

      ["<tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_next_item()
          -- You could replace the expand_or_jumpable() calls with
          -- expand_or_locally_jumpable() they way you will only jump inside
          -- the snippet region
        elseif luasnip.expand_or_jumpable() then
          luasnip.expand_or_jump()
        elseif has_words_before() then
          cmp.complete()
        else
          fallback()
        end
      end, { "i", "s" }),

      ["<s-tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_prev_item()
        elseif luasnip.jumpable(-1) then
          luasnip.jump(-1)
        else
          fallback()
        end
      end, { "i", "s" }),
      ["<cr>"] = cmp.mapping.confirm({ select = true }),
    })

    opts.sources = cmp.config.sources({
      { name = "nvim_lsp" },
      { name = "luasnip" },
      { name = "buffer" },
    }, { name = "buffer" })
  end,
}
