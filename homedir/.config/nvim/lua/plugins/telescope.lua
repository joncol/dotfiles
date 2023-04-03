return {
  {
    "nvim-telescope/telescope.nvim",
    lazy = false,
    dependencies = {
      "nvim-lua/plenary.nvim",
    },

    keys = {
      {
        "<leader>fi",
        function()
          require("telescope.builtin").find_files({
            cwd = "~/.config/nvim",
            follow = true,
          })
        end,
        desc = "Find init file",
      },
      {
        "<leader>fp",
        function()
          require("telescope.builtin").find_files({
            cwd = require("lazy.core.config").options.root,
          })
        end,
        desc = "Find plugin file",
      },
    },

    config = function()
      require("telescope").setup()

      local builtin = require("telescope.builtin")
      vim.lsp.handlers["textDocument/definition"] = builtin.lsp_definitions
      vim.lsp.handlers["textDocument/typeDefinition"] =
        builtin.lsp_type_definitions
      vim.lsp.handlers["textDocument/references"] = builtin.lsp_references
      vim.lsp.handlers["textDocument/documentSymbol"] =
        builtin.lsp_document_symbols
      vim.lsp.handlers["workspace/symbol"] = builtin.lsp_workspace_symbols

      vim.keymap.set(
        "n",
        "<leader>ff",
        builtin.find_files,
        { desc = "Find file" }
      )
      vim.keymap.set("n", "<leader>fg", builtin.live_grep, { desc = "Grep" })
      vim.keymap.set(
        "n",
        "<leader>b",
        builtin.buffers,
        { noremap = true, desc = "Find buffer" }
      )
      vim.keymap.set(
        "n",
        "<leader>fh",
        builtin.help_tags,
        { noremap = true, desc = "Find help tag" }
      )
      vim.keymap.set(
        "n",
        "<leader>fr",
        builtin.oldfiles,
        { noremap = true, desc = "Find recent files" }
      )
      vim.api.nvim_set_keymap(
        "n",
        "<leader>fb",
        ":Telescope file_browser path=%:p:h select_buffer=true<cr>",
        { noremap = true, desc = "Browse files" }
      )
    end,
  },

  {
    "telescope.nvim",
    dependencies = {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "make",
      config = function()
        require("telescope").load_extension("fzf")
      end,
    },
  },

  {
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
    config = function()
      require("telescope").load_extension("file_browser")
    end,
  },
}
