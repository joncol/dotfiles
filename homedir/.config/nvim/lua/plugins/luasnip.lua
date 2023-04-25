return {
  "L3MON4D3/LuaSnip",
  config = function()
    local ls = require("luasnip")
    local s = ls.snippet
    -- local sn = ls.snippet_node
    -- local isn = ls.indent_snippet_node
    local t = ls.text_node
    local i = ls.insert_node
    local f = ls.function_node
    -- local c = ls.choice_node
    -- local d = ls.dynamic_node
    -- local r = ls.restore_node
    -- local events = require("luasnip.util.events")
    -- local ai = require("luasnip.nodes.absolute_indexer")
    -- local extras = require("luasnip.extras")
    -- local l = extras.lambda
    -- local rep = extras.rep
    -- local p = extras.partial
    -- local m = extras.match
    -- local n = extras.nonempty
    -- local dl = extras.dynamic_lambda
    -- local fmt = require("luasnip.extras.fmt").fmt
    -- local fmta = require("luasnip.extras.fmt").fmta
    -- local conds = require("luasnip.extras.expand_conditions")
    -- local postfix = require("luasnip.extras.postfix").postfix
    -- local types = require("luasnip.util.types")
    -- local parse = require("luasnip.util.parser").parse_snippet
    -- local ms = ls.multi_snippet
    --
    ls.add_snippets("all", {
      s("ternary", {
        -- equivalent to "${1:cond} ? ${2:then} : ${3:else}"
        i(1, "cond"),
        t(" ? "),
        i(2, "then"),
        t(" : "),
        i(3, "else"),
      }),
    })

    ls.add_snippets("haskell", {
      s("f", { t("focus $ ") }),
      s("lg", { t('logInfo_ "'), i(0), t('"') }),
      s("lo", {
        t('logInfo_ $ "'),
        f(function(args)
          return args[1][1]
        end, { 1 }),
        t(': " <> showtp ('),
        i(1),
        t(")"),
      }),
    })

    local javascript_and_react_snippets = {
      s("clg", { t('console.log("'), i(0), t('")') }),
      s("clo", {
        t('console.log("'),
        f(function(args)
          return args[1][1]
        end, { 1 }),
        t(':", '),
        i(1),
        t(")"),
      }),
    }

    ls.add_snippets("javascript", javascript_and_react_snippets)
    ls.add_snippets("javascriptreact", javascript_and_react_snippets)

    ls.add_snippets("lua", {
      s({ trig = "key", name = "Map a key" }, {
        t("vim.keymap.set("),
        i(1, "mode"),
        t(", "),
        i(2, "lhs"),
        t(", "),
        i(3, "rhs"),
        t(", "),
        i(4, "opts"),
        t(")"),
      }),
      s("le", { t("<leader>") }),
      s("ll", { t("<localleader>") }),
      s("deps", { t({ "dependencies = {" }), i(0), t("},") }),
    })
  end,
}
