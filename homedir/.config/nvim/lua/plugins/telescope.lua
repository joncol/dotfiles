return {
  {
    "nvim-telescope/telescope.nvim",

    lazy = false,

    dependencies = {
      "nvim-lua/plenary.nvim",
    },

    config = function()
      local trouble = require("trouble.providers.telescope")
      local builtin = require("telescope.builtin")
      local extensions = require("telescope").extensions

      require("telescope").setup({
        defaults = {
          mappings = {
            i = { ["<c-t>"] = trouble.open_with_trouble },
            n = { ["<c-t>"] = trouble.open_with_trouble },
          },
        },
        extensions = {
          file_browser = {},
          media_files = {
            find_cmd = "rg",
          },
        },
      })

      vim.lsp.handlers["textDocument/definition"] = builtin.lsp_definitions
      vim.lsp.handlers["textDocument/typeDefinition"] =
        builtin.lsp_type_definitions
      vim.lsp.handlers["textDocument/references"] = builtin.lsp_references
      vim.lsp.handlers["textDocument/documentSymbol"] =
        builtin.lsp_document_symbols
      vim.lsp.handlers["workspace/symbol"] = builtin.lsp_workspace_symbols

      vim.keymap.set("n", "<leader>fi", function()
        builtin.find_files({
          cwd = "~/.config/nvim",
          follow = true,
        })
      end, { desc = "Find init file" })

      vim.keymap.set("n", "<leader>fp", function()
        extensions.file_browser.file_browser({
          path = vim.fn.stdpath("config") .. "/lua/plugins",
        })
      end, { noremap = true, desc = "Find plugin file" })

      vim.keymap.set("n", "<leader>fl", function()
        builtin.find_files({
          cwd = require("lazy.core.config").options.root,
        })
      end, { desc = "Find plugin file" })

      vim.keymap.set("n", "<leader>fF", ":Telescope find_files hidden=true<cr>")
      vim.keymap.set("n", "<leader>ff", ":Telescope find_files<cr>")
      vim.keymap.set("n", "<localleader>r", ":Telescope live_grep<cr>")
      vim.keymap.set("n", "<leader>b", ":Telescope buffers<cr>")
      vim.keymap.set("n", "<leader>fh", ":Telescope help_tags<cr>")
      vim.keymap.set("n", "<leader>fr", ":Telescope oldfiles<cr>")
      vim.keymap.set("n", "<leader>fo", ":Telescope vim_options<cr>")
      vim.keymap.set("n", "<leader>fP", ":Telescope planets<cr>")
      vim.keymap.set("n", "<leader>fm", ":Telescope media_files<cr>")
      vim.keymap.set(
        "n",
        "<leader>fb",
        ":Telescope current_buffer_fuzzy_find<cr>"
      )
      vim.keymap.set("n", "<leader>fG", ":Telescope git_commits<cr>")
      vim.keymap.set("n", "<leader>fg", ":Telescope git_bcommits<cr>")
    end,
  },

  {
    "telescope.nvim",
    dependencies = {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "make",
      config = function()
        require("telescope").load_extension("fzf")

        vim.keymap.set(
          "n",
          "<leader>f.",
          ":Telescope file_browser path=%:p:h select_buffer=true<cr>",
          { noremap = true, desc = "Browse files" }
        )
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

  {
    "nvim-telescope/telescope-media-files.nvim",
    dependencies = {
      "nvim-telescope/telescope.nvim",
      "nvim-lua/plenary.nvim",
      "nvim-lua/popup.nvim",
    },
    config = function()
      require("telescope").load_extension("media_files")
    end,
  },
}
