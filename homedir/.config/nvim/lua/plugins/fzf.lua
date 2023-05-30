return {
  "ibhagwan/fzf-lua",
  config = function()
    vim.env.FZF_DEFAULT_OPTS = nil

    vim.keymap.set(
      "n",
      "<Leader>zb",
      "<Cmd>lua require('fzf-lua').buffers()<CR>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<Leader>zf",
      "<Cmd>lua require('fzf-lua').files()<CR>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<Leader>zr",
      "<Cmd>lua require('fzf-lua').oldfiles()<CR>",
      { silent = true }
    )

    local path = require("fzf-lua.path")
    vim.keymap.set("n", "<leader>fi", function()
      local plugins_dir = vim.fn.stdpath("config") .. "/lua/plugins"
      require("fzf-lua").files({
        cwd = plugins_dir,
        actions = {
          ["default"] = function(selected, opts)
            local selected_item = selected[1]
            local status, entry =
              pcall(path.entry_to_file, selected_item, opts, opts.force_uri)
            local last_query =
              require("fzf-lua").config.__resume_data.last_query
            if selected_item and status and vim.loop.fs_stat(entry.path) then
              require("fzf-lua").actions.file_edit(selected, opts)
            else
              vim.cmd("e " .. plugins_dir .. "/" .. last_query)
            end
          end,
        },
      })
    end, { noremap = true, desc = "Find init file" })
    vim.keymap.set(
      "n",
      "<Leader>zr",
      "<Cmd>lua require('fzf-lua').live_grep_glob()<CR>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<Leader>zR",
      "<Cmd>lua require('fzf-lua').live_grep_resume()<CR>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<Leader>zq",
      "<Cmd>lua require('fzf-lua').quickfix()<CR>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<Leader>zQ",
      "<Cmd>lua require('fzf-lua').quickfix_stack()<CR>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<Leader>zl",
      "<Cmd>lua require('fzf-lua').loclist()<CR>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<Leader>zl",
      "<Cmd>lua require('fzf-lua').loclist_stack()<CR>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<Leader>zG",
      "<Cmd>lua require('fzf-lua').git_commits()<CR>",
      { silent = true }
    )
    vim.keymap.set(
      "n",
      "<Leader>zg",
      "<Cmd>lua require('fzf-lua').git_bcommits()<CR>",
      { silent = true }
    )
  end,
}
