return {
  "mhartington/formatter.nvim",

  config = function()
    local util = require("formatter.util")

    require("formatter").setup({
      filetype = {
        haskell = {
          function()
            return {
              exe = "fourmolu",
              args = {
                "--stdin-input-file",
                util.escape_path(util.get_current_buffer_file_path()),
                "--",
                "-",
              },
              stdin = true,
            }
          end,
        },

        lua = {
          function()
            return {
              exe = "stylua",
              args = {
                "--search-parent-directories",
                "--stdin-filepath",
                util.escape_path(util.get_current_buffer_file_path()),
                "--",
                "-",
              },
              stdin = true,
            }
          end,
        },

        nix = {
          function()
            return {
              exe = "nixfmt",
              stdin = true,
            }
          end,
        },

        ["*"] = {
          require("formatter.filetypes.any").remove_trailing_whitespace,
        },
      },
    })

    vim.keymap.set("n", "<localleader>F", ":Format<cr>", { desc = "Format" })

    -- Autoformat on save.
    vim.cmd([[
      augroup FormatAutogroup
      autocmd!
      autocmd BufWritePost * FormatWrite
      augroup END
    ]])
  end,
}
