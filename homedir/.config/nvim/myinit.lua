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
vim.g.maplocalleader = ","

require("lazy").setup("plugins")

vim.opt.colorcolumn = "80"
vim.o.number = true
vim.o.relativenumber = true
vim.o.signcolumn = "yes" -- Always leave room for gitsigns.

vim.keymap.set("n", "Y", "Y")
vim.keymap.set("x", "<leader>p", [["0p]])
vim.keymap.set("n", "<leader>P", [[viw"0p]])

vim.cmd.colorscheme "catppuccin-mocha"
-- vim.cmd.colorscheme "carbonfox"
-- vim.cmd.colorscheme "dawnfox"
-- vim.cmd.colorscheme "duskfox"
