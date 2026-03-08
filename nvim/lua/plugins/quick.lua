vim.api.nvim_create_user_command("Run", function(opts)
    local cmd = opts.args
    local lines = {}

    vim.fn.jobstart(cmd, {
        stderr_buffered = true,
        stdout_buffered = true,

        on_stdout = function(_, data)
            if data then
                vim.list_extend(lines, data)
            end
        end,

        on_stderr = function(_, data)
            if data then
                vim.list_extend(lines, data)
            end
        end,

        on_exit = function()
            vim.fn.setqflist({}, ' ', {
                title = cmd,
                lines = lines,
                efm = "%f:%l:%c: %m"
            })
            vim.cmd("copen")
        end,
    })
end, { nargs = "+" })
