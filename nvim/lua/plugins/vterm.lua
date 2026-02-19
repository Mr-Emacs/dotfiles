local terminal_buf = nil

local function open_terminal(split_type)
    if terminal_buf and vim.api.nvim_buf_is_valid(terminal_buf) then
        for _, win in ipairs(vim.api.nvim_list_wins()) do
            if vim.api.nvim_win_get_buf(win) == terminal_buf then
                vim.api.nvim_set_current_win(win)
                return
            end
        end
        vim.cmd(split_type)
        vim.api.nvim_win_set_buf(0, terminal_buf)
    else
        vim.cmd(split_type)
        vim.cmd('terminal')
        terminal_buf = vim.api.nvim_get_current_buf()

        vim.api.nvim_buf_attach(terminal_buf, false, {
            on_detach = function()
                terminal_buf = nil
            end
        })
    end
end

vim.api.nvim_create_user_command('HSplitTerminal', function() open_terminal('split') end, {})
vim.api.nvim_create_user_command('VSplitTerminal', function() open_terminal('vsplit') end, {})
