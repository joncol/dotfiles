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
vim.g.maplocalleader = "\\"

require("lazy").setup("plugins")

vim.opt.colorcolumn = "80"
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

vim.keymap.set("n", "Y", "Y")
vim.keymap.set("x", "<leader>p", [["0p]])
vim.keymap.set("n", "<leader>P", [[viw"0p]])

vim.api.nvim_create_autocmd("BufEnter", {
  -- Don't make newly inserted lines after a comment into a comment.
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

vim.keymap.set({ "n", "x" }, "<C-x><space>", ":noh<cr>")
vim.keymap.set({ "n", "x" }, "<C-x><C-space>", ":noh<cr>")

vim.cmd.colorscheme("catppuccin-mocha")
-- vim.cmd.colorscheme "carbonfox"
-- vim.cmd.colorscheme "dawnfox"
-- vim.cmd.colorscheme "duskfox"
