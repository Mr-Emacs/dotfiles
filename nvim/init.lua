-- Settings
vim.cmd('luafile ~/.config/nvim/gruber.lua')
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.swapfile = false
vim.opt.signcolumn = "yes"
vim.opt.tabstop = 4
vim.g.mapleader = " "
vim.o.cursorline = true
vim.o.winborder = "rounded"

vim.keymap.set({ 'n', 'v', 'x' }, '<leader>y', '"+y<CR>')
vim.keymap.set({ 'n', 'v', 'x' }, '<leader>d', '"+d<CR>')

vim.keymap.set('n', '<leader>r', ':update<CR> :source<CR>')
vim.keymap.set('n', '<leader>w', ':w<CR>')
vim.keymap.set('n', '<leader>q', ':q<CR>')

vim.cmd("set completeopt+=noselect")
vim.keymap.set('n', '<leader>o', ':Ex<CR>')
vim.keymap.set('n', '<leader>g', ':Git<CR>')

-- Plugins
require('mini.pick').setup {}
vim.keymap.set('n', '<leader>pf', ':Pick files<CR>')
vim.keymap.set('n', '<leader>ps', ':Pick grep<CR>')
vim.keymap.set('n', '<leader>bb', ':Pick buffers<CR>')
vim.keymap.set('n', '<leader>h', ':Pick help<CR>')
-- LSP
vim.lsp.enable({ "clangd", "lua_ls", "ts_ls", "gopls" })
vim.keymap.set('n', '<leader>lf', vim.lsp.buf.format)
vim.api.nvim_set_keymap('n', '<leader>gd', '<cmd>lua vim.diagnostic.open_float()<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>R', '<cmd>lua vim.lsp.buf.rename()<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fq', '<cmd>lua vim.lsp.buf.code_action()<CR>', { noremap = true, silent = true })

require('typst-preview').setup {}
vim.keymap.set('n', '<leader>mp', ':TypstPreview<CR>')
