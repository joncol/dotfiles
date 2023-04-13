return {
  "neovim/nvim-lspconfig",
  init = function()
    vim.keymap.set("n", "<A-.>", "", {})
  end,
  config = function()
    local lspconfig = require("lspconfig")
    lspconfig.lua_ls.setup({})
    lspconfig.hls.setup({})

    vim.api.nvim_create_autocmd("LspAttach", {
      group = vim.api.nvim_create_augroup(
        "my-lsp-attach-format",
        { clear = true }
      ),
      callback = function(args)
        -- Enable completion triggered by <c-x><c-o>.
        vim.bo[args.buf].omnifunc = "v:lua.vim.lsp.omnifunc"

        local opts = { buffer = args.buf }
        vim.keymap.set(
          "n",
          "gD",
          vim.lsp.buf.declaration,
          vim.tbl_extend("force", opts, { desc = "Go to declaration" })
        )
        vim.keymap.set(
          "n",
          "gd",
          vim.lsp.buf.definition,
          vim.tbl_extend("force", opts, { desc = "Go to definition" })
        )
        vim.keymap.set(
          "n",
          "<A-.>",
          vim.lsp.buf.definition,
          vim.tbl_extend("force", opts, { desc = "Go to definition" })
        )
        vim.keymap.set(
          "n",
          "K",
          vim.lsp.buf.hover,
          vim.tbl_extend("force", opts, { desc = "Hover" })
        )
        vim.keymap.set(
          "n",
          "gi",
          vim.lsp.buf.implementation,
          vim.tbl_extend("force", opts, { desc = "Go to implementation" })
        )
        vim.keymap.set(
          "n",
          "<C-k>",
          vim.lsp.buf.signature_help,
          vim.tbl_extend("force", opts, { desc = "Signature help" })
        )
        vim.keymap.set(
          "n",
          "<leader>wa",
          vim.lsp.buf.add_workspace_folder,
          vim.tbl_extend("force", opts, { desc = "Add workspace folder" })
        )
        vim.keymap.set(
          "n",
          "<leader>wr",
          vim.lsp.buf.remove_workspace_folder,
          vim.tbl_extend("force", opts, { desc = "Remove workspace folder" })
        )
        vim.keymap.set("n", "<leader>wl", function()
          print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        end, vim.tbl_extend(
          "force",
          opts,
          { desc = "List workspace folders" }
        ))
        vim.keymap.set(
          "n",
          "<leader>lr",
          vim.lsp.buf.rename,
          vim.tbl_extend("force", opts, { desc = "Rename" })
        )
        vim.keymap.set(
          "n",
          "<leader>la",
          vim.lsp.buf.code_action,
          vim.tbl_extend("force", opts, { desc = "Code action" })
        )
        vim.keymap.set(
          "n",
          "<leader>lgr",
          vim.lsp.buf.references,
          vim.tbl_extend("force", opts, { desc = "Find references" })
        )
        vim.keymap.set(
          "n",
          "<leader>lgt",
          vim.lsp.buf.type_definition,
          vim.tbl_extend("force", opts, { desc = "Go to type of" })
        )
        vim.keymap.set(
          "n",
          "<leader>ld",
          vim.lsp.buf.document_symbol,
          vim.tbl_extend("force", opts, { desc = "Document symbols" })
        )
        vim.keymap.set(
          "n",
          "<leader>lw",
          vim.lsp.buf.workspace_symbol,
          vim.tbl_extend("force", opts, { desc = "Workspace symbols" })
        )
        vim.keymap.set("n", "<leader>l==", function()
          vim.lsp.buf.format({ async = true })
        end, vim.tbl_extend("force", opts, { desc = "Format file" }))
      end,
    })
  end,
}
