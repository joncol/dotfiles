local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

vim.g.mapleader = ","

-- Make it possible to repeat latest search backwards on current line.
vim.keymap.set("n", ",,", ",", { noremap = true })

vim.g.maplocalleader = " "

require("lazy").setup("plugins", {
  change_detection = {
    enabled = false,
    notify = false,
  },
})

vim.keymap.set("n", "<leader>ll", ":Lazy<cr>")

vim.o.textwidth = 80
vim.o.number = true
vim.o.relativenumber = true
vim.o.signcolumn = "yes" -- Always leave room for gitsigns.
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.scrolloff = 3
vim.o.splitright = true
vim.o.expandtab = true
vim.o.shiftwidth = 2
vim.o.wrap = false
vim.o.pumheight = 40
vim.opt.colorcolumn = "+1"
vim.opt.iskeyword:append("-") -- Do not treat hyphens as word separators.
vim.opt.cursorline = true

vim.keymap.set("n", "Y", "Y")
vim.keymap.set("x", "<leader>p", [["0p]])
vim.keymap.set("n", "<leader>P", [[viw"0p]])

-- Yank filename and path of current file.
vim.keymap.set("n", "<leader>yp", [[<Cmd>let @+=expand("%:~:h")<CR>]])
vim.keymap.set("n", "<leader>yP", [[<Cmd>let @+=expand("%:h")<CR>]])
vim.keymap.set("n", "<leader>yf", [[<Cmd>let @+=expand("%:t")<CR>]])
vim.keymap.set("n", "<leader>yF", [[<Cmd>let @+=expand("%:p")<CR>]])
vim.keymap.set("n", "<leader>yn", [[<Cmd>let @+=expand("%:~")<CR>]])

-- Autosave non-empty files.
vim.api.nvim_create_autocmd({ "BufLeave", "FocusLost" }, {
  command = [[if expand("%:e") != "log" && line2byte(line('$')) > 0 | silent! w | endif]],
})

-- Always open help windows in vertical splits.
vim.api.nvim_create_autocmd("BufWinEnter", {
  callback = function()
    if vim.bo.filetype == "help" then
      -- Ignore any errors from `:wincmd L`. Errors happen when opening a help
      -- page from within a help page.
      pcall(function()
        vim.cmd("wincmd L")
      end)
    end
  end,
})

-- Don't make newly inserted lines after a comment into a comment.
vim.api.nvim_create_autocmd("BufEnter", {
  callback = function()
    vim.opt.formatoptions:remove({ "c", "r", "o" })
  end,
})

-- Change binding for `cedit`. This avoids conflict with using `C-f` to move
-- forwards in the command line.
vim.o.cedit = "<C-y>"

-- Enable Emacs keys in the "minibuffer" (see: `:help emacs-keys`).
vim.cmd([[
  cnoremap <C-a> <Home>
  cnoremap <C-b> <Left>
  cnoremap <C-d> <Del>
  cnoremap <C-e> <End>
  cnoremap <C-f> <Right>
  cnoremap <C-n> <Down>
  cnoremap <C-p> <Up>
  cnoremap <A-b> <S-Left>
  cnoremap <A-f> <S-Right>
]])

-- Avoid scrolling when changing buffers.
-- See: https://vim.fandom.com/wiki/Avoid_scrolling_when_switch_buffers
vim.cmd([[
  " Save current view settings on a per-window, per-buffer basis.
  function! AutoSaveWinView()
    if !exists("w:SavedBufView")
      let w:SavedBufView = {}
    endif
    let w:SavedBufView[bufnr("%")] = winsaveview()
  endfunction

  " Restore current view settings.
  function! AutoRestoreWinView()
    let buf = bufnr("%")
    if exists("w:SavedBufView") && has_key(w:SavedBufView, buf)
      let v = winsaveview()
      let atStartOfFile = v.lnum == 1 && v.col == 0
      if atStartOfFile && !&diff
        call winrestview(w:SavedBufView[buf])
      endif
      unlet w:SavedBufView[buf]
    endif
  endfunction

  " When switching buffers, preserve window view.
  if v:version >= 700
    autocmd BufLeave * call AutoSaveWinView()
    autocmd BufEnter * call AutoRestoreWinView()
  endif
]])

-- Navigating between diagnostics
vim.keymap.set("n", "[d", "<Cmd>lua vim.diagnostic.goto_prev()<CR>")
vim.keymap.set("n", "]d", "<Cmd>lua vim.diagnostic.goto_next()<CR>")

vim.cmd.colorscheme("catppuccin-mocha")
-- vim.cmd.colorscheme "carbonfox"
-- vim.cmd.colorscheme "dawnfox"
-- vim.cmd.colorscheme "duskfox"
