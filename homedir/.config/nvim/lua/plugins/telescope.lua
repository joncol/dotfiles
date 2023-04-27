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
        pickers = {
          colorscheme = {
            enable_preview = true,
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

      vim.keymap.set("n", "<leader>fI", function()
        extensions.file_browser.file_browser({
          path = vim.fn.stdpath("config") .. "/lua/plugins",
        })
      end, { noremap = true, desc = "Find plugin file" })

      vim.keymap.set("n", "<leader>fl", function()
        builtin.find_files({
          cwd = require("lazy.core.config").options.root,
        })
      end, { desc = "Find plugin file" })

      vim.keymap.set(
        "n",
        "<leader>fF",
        "<Cmd>Telescope find_files hidden=true<CR>"
      )
      vim.keymap.set("n", "<leader>ff", "<Cmd>Telescope find_files<CR>")
      vim.keymap.set("n", "<localleader>r", "<Cmd>Telescope live_grep<CR>")
      vim.keymap.set("n", "<leader>b", "<Cmd>Telescope buffers<CR>")
      vim.keymap.set("n", "<leader>fh", "<Cmd>Telescope help_tags<CR>")
      vim.keymap.set("n", "<leader>fr", "<Cmd>Telescope oldfiles<CR>")
      vim.keymap.set("n", "<leader>fo", "<Cmd>Telescope vim_options<CR>")
      vim.keymap.set("n", "<leader>fP", "<Cmd>Telescope planets<CR>")
      vim.keymap.set("n", "<leader>fm", "<Cmd>Telescope media_files<CR>")
      vim.keymap.set(
        "n",
        "<leader>fb",
        "<Cmd>Telescope current_buffer_fuzzy_find<CR>"
      )
      vim.keymap.set("n", "<leader>fG", "<Cmd>Telescope git_commits<CR>")
      vim.keymap.set("n", "<leader>fg", "<Cmd>Telescope git_bcommits<CR>")
      vim.keymap.set("n", "<leader>fk", "<Cmd>Telescope keymaps<CR>")
      vim.keymap.set("n", "<leader>fc", "<Cmd>Telescope colorscheme<CR>")
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
          "<Cmd>Telescope file_browser path=%:p:h select_buffer=true<CR>",
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

  {
    "nvim-telescope/telescope-live-grep-args.nvim",
    config = function()
      vim.keymap.set(
        "n",
        "<localleader>R",
        ":lua require('telescope').extensions.live_grep_args.live_grep_args()<CR>"
      )
    end,
  },
}
