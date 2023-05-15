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

    ls.add_snippets("all", {
      s("envrc", {
        t({
          "use flake -j12",
          "",
          "if [ -e .envrc-local ]; then",
          "  source .envrc-local",
          "fi",
        }),
      }),
      s("flake", {
        t({
          "{",
          '  description = "Flake utils example";',
          "",
          "  inputs = {",
          '    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";',
          '    flake-utils.url = "github:numtide/flake-utils";',
          "  };",
          "",
          "  outputs = { self, nixpkgs, flake-utils }:",
          "    flake-utils.lib.eachDefaultSystem (system:",
          "      let",
          "        overlays = [ ];",
          "        pkgs = builtins.foldl' (acc: overlay: acc.extend overlay)",
          "          nixpkgs.legacyPackages.${system} overlays;",
          "      in rec {",
          "        packages = flake-utils.lib.flattenTree {",
          "          hello = pkgs.hello;",
          "          gitAndTools = pkgs.gitAndTools;",
          "        };",
          "        defaultPackage = packages.hello;",
          "        apps.hello = flake-utils.lib.mkApp { drv = packages.hello; };",
          "        defaultApp = apps.hello;",
          "        devShells.default =",
          "          pkgs.mkShell rec { packages = with pkgs; [ nyancat ]; };",
          "      });",
          "}",
        }),
      }),
      s("justfile", {
        t({
          "# Local Variables:",
          "# mode: makefile",
          "# indent-tabs-mode: nil",
          "# End:",
          "# vim: set ft=make expandtab shiftwidth=2 tabstop=2:",
        }),
      }),
    })

    ls.add_snippets("haskell", {
      s("af", { t('assertBool "Dummy" False') }),
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
      s("q", { t("qualified") }),
      s("t", { t("-- TODO: "), i(0) }),
      s("tj", { t("-- TODO_JCO: "), i(0) }),
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
      s("<l", { t("<Leader>") }),
      s("<ll", { t("<LocalLeader>") }),
      s("deps", { t({ "dependencies = {" }), i(0), t("},") }),
    })
  end,
}
