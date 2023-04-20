return {
  {
    "folke/neodev.nvim",
  },

  {
    "neovim/nvim-lspconfig",

    config = function()
      require("neodev").setup()

      require("lspconfig").lua_ls.setup({
        single_file_support = true,
        settings = {
          Lua = {
            diagnostics = {
              globals = { "vim" },
            },
            workspace = {
              checkThirdParty = false,
              library = vim.api.nvim_get_runtime_file("", true),
            },
            telemetry = {
              enable = false,
            },
          },
        },
      })
      require("lspconfig").hls.setup({})
      require("lspconfig").elmls.setup({})

      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup(
          "my-lsp-attach-format",
          { clear = true }
        ),
        callback = function(args)
          local opts = { buffer = args.buf }
          local keymap = vim.keymap.set
          keymap(
            "n",
            "gD",
            vim.lsp.buf.declaration,
            vim.tbl_extend("force", opts, { desc = "Go to declaration" })
          )
          keymap(
            "n",
            "gd",
            vim.lsp.buf.definition,
            vim.tbl_extend("force", opts, { desc = "Go to definition" })
          )
          keymap(
            "n",
            "<A-.>",
            vim.lsp.buf.definition,
            vim.tbl_extend("force", opts, { desc = "Go to definition" })
          )
          keymap(
            "n",
            "K",
            vim.lsp.buf.hover,
            vim.tbl_extend("force", opts, { desc = "Hover" })
          )
          keymap(
            "n",
            "gi",
            vim.lsp.buf.implementation,
            vim.tbl_extend("force", opts, { desc = "Go to implementation" })
          )
          keymap(
            "n",
            "<C-k>",
            vim.lsp.buf.signature_help,
            vim.tbl_extend("force", opts, { desc = "Signature help" })
          )
          keymap(
            "n",
            "<leader>wa",
            vim.lsp.buf.add_workspace_folder,
            vim.tbl_extend("force", opts, { desc = "Add workspace folder" })
          )
          keymap(
            "n",
            "<leader>wr",
            vim.lsp.buf.remove_workspace_folder,
            vim.tbl_extend("force", opts, { desc = "Remove workspace folder" })
          )
          keymap(
            "n",
            "<leader>wl",
            function()
              print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
            end,
            vim.tbl_extend("force", opts, { desc = "List workspace folders" })
          )
          keymap(
            "n",
            "<leader>lr",
            vim.lsp.buf.rename,
            vim.tbl_extend("force", opts, { desc = "Rename" })
          )
          keymap(
            "n",
            "<leader>la",
            vim.lsp.buf.code_action,
            vim.tbl_extend("force", opts, { desc = "Code action" })
          )
          keymap(
            "n",
            "<leader>lgr",
            vim.lsp.buf.references,
            vim.tbl_extend("force", opts, { desc = "Find references" })
          )
          keymap(
            "n",
            "<leader>lgt",
            vim.lsp.buf.type_definition,
            vim.tbl_extend("force", opts, { desc = "Go to type of" })
          )
          keymap(
            "n",
            "<leader>ld",
            vim.lsp.buf.document_symbol,
            vim.tbl_extend("force", opts, { desc = "Document symbols" })
          )
          keymap(
            "n",
            "<leader>lw",
            vim.lsp.buf.workspace_symbol,
            vim.tbl_extend("force", opts, { desc = "Workspace symbols" })
          )
          keymap("n", "<leader>l==", function()
            vim.lsp.buf.format({ async = true })
          end, vim.tbl_extend("force", opts, { desc = "Format file" }))
        end,
      })
    end,
  },
}
