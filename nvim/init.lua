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


-- For testing Purposes
 vim.filetype.add({
 	extension = {
 		code = "code",
 	},
 })

-- File type detection for vlogcc
vim.api.nvim_create_autocmd('BufNewFile', {
    pattern = { '*.code' },
    callback = function()
        vim.bo.filetype = 'vlogcc'
    end
})

vim.api.nvim_create_autocmd('BufRead', {
    pattern = { '*.code' },
    callback = function()
        vim.bo.filetype = 'vlogcc'
    end
})

-- Setup custom vlogcc LSP server using raw LSP API
vim.api.nvim_create_autocmd('LspAttach', {
    group = vim.api.nvim_create_augroup('VlogccLsp', {}),
    callback = function(args)
        local client = vim.lsp.get_client_by_id(args.data.client_id)
        if client.name == 'vlogcc' then
            -- Set up keymaps for vlogcc LSP
            local opts = { buffer = args.buf }
            vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
            vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
            vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
        end
    end
})

-- Start the vlogcc LSP server
vim.api.nvim_create_autocmd('FileType', {
    pattern = { 'vlogc', 'code' },
    callback = function()
        vim.lsp.start({
            name = 'vlogc',
            cmd = { "/home/xsoder/programming/vlogc-lsp/main" },
            root_dir = vim.fs.dirname(vim.fs.find({ '*.code', 'go.mod' }, { upward = true })[1] or vim.fn.getcwd()),
        })
    end
})
