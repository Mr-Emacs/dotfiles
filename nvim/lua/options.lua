vim.opt.number = true
vim.opt.wrap = true
vim.opt.smartindent = true
vim.opt.autoindent = true
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.clipboard = "unnamedplus"

vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.cursorline = true
vim.opt.showmatch = true
vim.opt.foldmethod = 'indent'
vim.opt.foldlevelstart = 99

vim.opt.lazyredraw = true
vim.opt.updatetime = 300
vim.opt.undofile = true

vim.g.netrw_banner = 0
vim.opt.number = true
vim.opt.termguicolors = true
vim.opt.guicursor = ""
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.autochdir = true

if vim.g.neovide then
    vim.o.guifont = "JetBrainsMono Nerd Font:h12"
    vim.cmd [[ autocmd VimEnter * cd /home/bortlov/Programming ]]
end
