-- Settings
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

-- Color scheme
vim.api.nvim_create_autocmd('LspAttach', {
	callback = function(ev)
		local client = vim.lsp.get_client_by_id(ev.data.client_id)
		if client:supports_method('textDocument/completion') then
			vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = true })
		end
	end,
})
vim.cmd("set completeopt+=noselect")
vim.cmd('colorscheme retrobox')
vim.keymap.set('n', '<leader>o', ':Ex<CR>')
vim.keymap.set('n', '<leader>g', ':Git<CR>')

-- Plugins
require('mini.pick').setup {}
vim.keymap.set('n', '<leader>f', ':Pick files<CR>')
vim.keymap.set('n', '<leader>bb', ':Pick buffers<CR>')
vim.keymap.set('n', '<leader>h', ':Pick help<CR>')


vim.lsp.enable({ "clangd", "lua_ls", "ts_ls"  })
vim.keymap.set('n', '<leader>lf', vim.lsp.buf.format)

require('typst-preview').setup {}
vim.keymap.set('n', '<leader>mp', ':TypstPreview<CR>')
