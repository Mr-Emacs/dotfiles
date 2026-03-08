vim.lsp.config('*', {
    root_markers = { '.git' }
})

vim.lsp.config.pyright = {
    cmd = { 'pyright-langserver', '--stdio' },
    filetypes = { 'python' },
    root_markers = { '.git', 'pyproject.toml', 'setup.py' },
}

vim.lsp.config.clangd = {
    cmd = { 'clangd' },
    filetypes = { 'c', 'cc', 'cpp' },
    root_markers = { '.git', 'compile_commands.json' },
}

vim.lsp.enable('clangd')
vim.lsp.enable('rust_analyzer')

vim.diagnostic.config({
    virtual_text = false,
    signs = true,
    underline = true,
    update_in_insert = false,
    severity_sort = true,
})

local signs = {
    Error = "E",
    Warn = "W",
    Hint = "H",
    Info = "I"
}

for type, icon in pairs(signs) do
    local hl = "DiagnosticSign" .. type
    vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end
-- show diagnostic popup after 400ms
vim.o.updatetime = 400

vim.api.nvim_create_autocmd("CursorHold", {
    callback = function()
        vim.diagnostic.open_float(nil, {
            focusable = false,
            border = "single",
            source = "if_many",
            scope = "cursor",
        })
    end
})
