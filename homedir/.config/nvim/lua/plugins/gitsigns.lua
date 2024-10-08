return {
  "lewis6991/gitsigns.nvim",
  opts = {
    on_attach = function(bufnr)
      local gs = package.loaded.gitsigns

      local function map(mode, l, r, opts)
        opts = opts or {}
        opts.buffer = bufnr
        vim.keymap.set(mode, l, r, opts)
      end

      map("n", "<localleader>gs", "<Cmd>Gitsigns toggle_signs<CR>")

      -- Navigation.
      map("n", "]c", function()
        if vim.wo.diff then
          return "]c"
        end
        vim.schedule(function()
          gs.next_hunk()
        end)
        return "<Ignore>"
      end, { expr = true })

      map("n", "[c", function()
        if vim.wo.diff then
          return "[c"
        end
        vim.schedule(function()
          gs.prev_hunk()
        end)
        return "<Ignore>"
      end, { expr = true })

      -- Actions.
      map({ "n", "v" }, "<localleader>hs", ":Gitsigns stage_hunk<cr>")
      map({ "n", "v" }, "<localleader>hr", ":Gitsigns reset_hunk<cr>")
      map("n", "<localleader>hS", gs.stage_buffer)
      map("n", "<localleader>hu", gs.undo_stage_hunk)
      map("n", "<localleader>hR", gs.reset_buffer)
      map("n", "<localleader>hp", gs.preview_hunk)
      map("n", "<localleader>hb", function()
        gs.blame_line({ full = true })
      end)
      map("n", "<localleader>tb", gs.toggle_current_line_blame)
      map("n", "<localleader>hd", gs.diffthis)
      map("n", "<localleader>hD", function()
        gs.diffthis("~")
      end)
      map("n", "<localleader>hx", gs.toggle_deleted)
      -- Text object.
      map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<cr>")
    end,
  },
}
