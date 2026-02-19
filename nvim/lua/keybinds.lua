vim.g.mapleader = " "

vim.g.neovide_scale_factor = vim.g.neovide_scale_factor or 1.0

if vim.g.neovide then
    vim.api.nvim_set_keymap('n', '<C-=>', ":lua vim.g.neovide_scale_factor = vim.g.neovide_scale_factor + 0.1<CR>", { noremap = true, silent = true })
    vim.api.nvim_set_keymap('n', '<C-->', ":lua vim.g.neovide_scale_factor = vim.g.neovide_scale_factor - 0.1<CR>", { noremap = true, silent = true })
    vim.api.nvim_set_keymap('n', '<C-0>', ":lua vim.g.neovide_scale_factor = 1<CR>", { noremap = true, silent = true })

    vim.api.nvim_set_keymap('v', '<C-=>', ":lua vim.g.neovide_scale_factor = vim.g.neovide_scale_factor + 0.1<CR>", { noremap = true, silent = true })
    vim.api.nvim_set_keymap('v', '<C-->', ":lua vim.g.neovide_scale_factor = vim.g.neovide_scale_factor - 0.1<CR>", { noremap = true, silent = true })
end

vim.api.nvim_set_keymap('n', '<leader>w', ':w<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>q', ':q<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>o', ':Yazi<CR>', { noremap = true, silent = true })

vim.api.nvim_set_keymap('n', '<leader>h', '<C-w>h', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>j', '<C-w>j', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>k', '<C-w>k', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>l', '<C-w>l', { noremap = true, silent = true })

vim.api.nvim_set_keymap('n', '<leader>y', '"+yy', { noremap = true, silent = true, desc = 'Copy to system clipboard' })

vim.api.nvim_set_keymap('n', '<leader>pf', ':Telescope find_files<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>bb', ':Telescope buffers<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>pg', ':Telescope git_files<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>ps', ':Telescope live_grep<CR>', { noremap = true, silent = true })

vim.keymap.set('n', 'gd', vim.lsp.buf.definition)
vim.keymap.set('n', 'K', vim.lsp.buf.hover)
vim.keymap.set('n', 'gr', vim.lsp.buf.references)
vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename)
vim.keymap.set('n', '<leader>fq', vim.lsp.buf.code_action)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Show diagnostic' })
vim.keymap.set('n', '<leader>fe', vim.diagnostic.setloclist, { desc = 'Diagnostic list' })
vim.keymap.set('n', '<leader>ff', vim.lsp.buf.format, { desc = 'Format buffer' })

vim.keymap.set("n", "<leader>pu", "<Cmd>UrlView<CR>", { desc = "View buffer URLs" })
vim.keymap.set("n", "<leader>u", "<Cmd>UndotreeToggle<CR>", { desc = "Show Undo files" })
vim.keymap.set("n", "<leader>x", "<cmd>!chmod +x %<CR>", { silent = true })
vim.keymap.set("n", "<leader>h", "<cmd>noh<CR>", { silent = true })
vim.api.nvim_set_keymap('t', '<Esc>', [[<C-\><C-n>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>ss', ':StripTrailingWhitespace<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>tt', ':VSplitTerminal<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader><leader>t', ':HSplitTerminal<CR>', { noremap = true, silent = true })
