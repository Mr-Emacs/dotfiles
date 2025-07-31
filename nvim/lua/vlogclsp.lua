local function start_vlogcc_lsp()
    local client_id = vim.lsp.start({
        name = "vlogcc",
        cmd = { "/home/xsoder/programming/vlogc-lsp/main" },
        root_dir = vim.fn.getcwd(), -- required for Neovim to initialize LSP
        on_init = function(client)
            vim.schedule(function()
                vim.notify("✅ VLOGCC LSP initialized (client_id=" .. client.id .. ")", vim.log.levels.INFO)
            end)
        end,
        on_exit = function(_, code, signal)
            vim.schedule(function()
                vim.notify("❌ VLOGCC LSP exited (code=" .. code .. ", signal=" .. signal .. ")", vim.log.levels.ERROR)
            end)
        end,
        on_stdout = function(_, output)
            vim.schedule(function()
                vim.notify("📤 LSP stdout: " .. tostring(output), vim.log.levels.INFO)
            end)
        end,
        on_stderr = function(_, err)
            vim.schedule(function()
                vim.notify("⚠️ LSP stderr: " .. tostring(err), vim.log.levels.ERROR)
            end)
        end,
    })

    if client_id then
        vim.lsp.buf_attach_client(0, client_id)
        vim.notify("🔌 VLOGCC LSP started and attached (client_id=" .. client_id .. ")", vim.log.levels.INFO)
    else
        vim.notify("❌ Failed to start VLOGCC LSP", vim.log.levels.ERROR)
    end
end

-- Auto-start on .code filetype
vim.api.nvim_create_autocmd("FileType", {
    pattern = "code",
    callback = function()
        start_vlogcc_lsp()
    end,
})

