vim.lsp.config('*', {
    root_markers = { '.git' }
})

vim.lsp.config.zls = {
    cmd = { 'zls' },
    filetypes = { 'zig' },
}

vim.lsp.config.lua_ls = {
    cmd = { 'lua-language-server' },
    filetypes = { 'lua' },
    root_markers = { '.git', '.luarc.json' },
    settings = {
        Lua = {
            diagnostics = {
                globals = { 'vim' }
            }
        }
    }
}

vim.lsp.config.rust_analyzer = {
  cmd = { 'rust-analyzer' },
  filetypes = { 'rust' },
  root_markers = { 'Cargo.toml', '.git' },
  settings = {
    ["rust-analyzer"] = {
      checkOnSave = true,        -- boolean enable
      check = {                  -- nested map for command/config
        overrideCommand = { "cargo", "clippy", "--message-format=json" },
        allTargets = true,
      },
      cargo = {
        allFeatures = true,
      },
    },
  },
}

vim.lsp.config.pyright = {
    cmd = { 'pyright-langserver', '--stdio' },
    filetypes = { 'python' },
    root_markers = { '.git', 'pyproject.toml', 'setup.py' },
}

vim.lsp.config.bash = {
    cmd = { 'bash-language-server' },
    filetypes = { 'sh' },
}

vim.lsp.config.ts_ls = {
    cmd = { 'typescript-language-server', '--stdio' },
    filetypes = { 'javascript', 'typescript', 'javascriptreact', 'typescriptreact' },
    root_markers = { '.git', 'package.json' },
}

vim.lsp.config.clangd = {
    cmd = { 'clangd' },
    filetypes = { 'c', 'cc', 'cpp' },
    root_markers = { '.git', 'compile_commands.json' },
}
vim.lsp.enable('lua_ls')
vim.lsp.enable('pyright')
vim.lsp.enable('ts_ls')
vim.lsp.enable('clangd')
vim.lsp.enable('rust_analyzer')
vim.lsp.enable('bash')
vim.lsp.enable('zls')

vim.diagnostic.config({
    virtual_text = true,
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

-- C/C++ Only Features
-- vim.api.nvim_create_autocmd('BufWritePre', {
--     pattern = { '*.c', '*.cc','*.cpp', '*.h', '*.hpp' },
--     callback = function()
--         vim.lsp.buf.format()
--     end
-- })
