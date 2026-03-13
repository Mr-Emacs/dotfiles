vim.opt.number = true
vim.opt.wrap = true
vim.opt.smartindent = true
vim.opt.autoindent = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
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
vim.opt.showtabline = 2
vim.opt.swapfile = false

if vim.g.neovide then
    local default_font = "JetBrainsMono_Nerd_Font:h11"
    local focus_font = "JetBrainsMono_Nerd_Font:h18"
    local focus_mode = false

    vim.o.guifont = default_font
    vim.opt.guicursor = "n-v-c:block,i:ver25"

    vim.api.nvim_create_user_command("EnterFocusMode", function()
        focus_mode = not focus_mode
        vim.o.guifont = focus_mode and focus_font or default_font
    end, {})
end

vim.api.nvim_set_hl(0, "CursorLine", { bg = "#00008B", fg = "none", bold = false, underline = false })
