return {
  "neovim/nvim-lspconfig",
  init = function()
    vim.keymap.set("n", "<A-.>", "", {})
  end,
  config = function()
    local lspconfig = require('lspconfig')
    lspconfig.lua_ls.setup {}
    lspconfig.hls.setup {}

    local format_is_enabled = true
    vim.api.nvim_create_user_command("AutoFormatToggle", function()
      format_is_enabled = not format_is_enabled
      print("Setting autoformatting to: " .. tostring(format_is_enabled))
    end, {})

    -- Create an augroup that is used for managing our formatting autocmds.
    -- We need one augroup per client to make sure that multiple clients
    -- can attach to the same buffer without interfering with each other.
    local _augroups = {}
    local get_augroup = function(client)
      if not _augroups[client.id] then
        local group_name = "my-lsp-format-" .. client.name
        local id = vim.api.nvim_create_augroup(group_name, { clear = true })
        _augroups[client.id] = id
      end

      return _augroups[client.id]
    end

    vim.api.nvim_create_autocmd("LspAttach", {
      group = vim.api.nvim_create_augroup("my-lsp-attach-format", { clear = true }),
      callback = function(args)
        local client_id = args.data.client_id
        local client = vim.lsp.get_client_by_id(client_id)
        local bufnr = args.buf

        -- Enable completion triggered by <c-x><c-o>.
        vim.bo[args.buf].omnifunc = "v:lua.vim.lsp.omnifunc"

        local opts = { buffer = args.buf }
        vim.keymap.set("n", "gD", vim.lsp.buf.declaration, vim.tbl_extend("force", opts, { desc = "Go to declaration" }))
        vim.keymap.set("n", "gd", vim.lsp.buf.definition, vim.tbl_extend("force", opts, { desc = "Go to definition" }))
        vim.keymap.set("n", "<A-.>", vim.lsp.buf.definition, vim.tbl_extend("force", opts, { desc = "Go to definition" }))
        vim.keymap.set("n", "K", vim.lsp.buf.hover, vim.tbl_extend("force", opts, { desc = "Hover" }))
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
        vim.keymap.set("n", "<leader>wa", vim.lsp.buf.add_workspace_folder,
          vim.tbl_extend("force", opts, { desc = "Add workspace folder" }))
        vim.keymap.set("n", "<leader>wr", vim.lsp.buf.remove_workspace_folder,
          vim.tbl_extend("force", opts, { desc = "Remove workspace folder" }))
        vim.keymap.set(
          "n",
          "<leader>wl",
          function()
            print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
          end,
          vim.tbl_extend("force", opts, { desc = "List workspace folders" })
        )
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
          "<leader>lds",
          vim.lsp.buf.document_symbol,
          vim.tbl_extend("force", opts, { desc = "Document symbols" })
        )
        vim.keymap.set(
          "n",
          "<leader>lws",
          vim.lsp.buf.workspace_symbol,
          vim.tbl_extend("force", opts, { desc = "Workspace symbols" })
        )
        vim.keymap.set(
          "n",
          "<leader>l==",
          function()
            vim.lsp.buf.format { async = true }
          end,
          vim.tbl_extend("force", opts, { desc = "Format file" })
        )

        -- Only attach to clients that support document formatting
        if not client.server_capabilities.documentFormattingProvider then
          return
        end

        if client.name == "tsserver" then
          return
        end

        -- Create an autocmd that will run *before* we save the buffer.
        -- Run the formatting command for the LSP that has just attached.
        vim.api.nvim_create_autocmd("BufWritePre", {
          group = get_augroup(client),
          buffer = bufnr,
          callback = function()
            if not format_is_enabled then
              return
            end

            vim.lsp.buf.format {
              async = false,
              filter = function(c)
                return c.id == client.id
              end,
            }
          end,
        })
      end,
    })
  end
}