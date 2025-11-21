local M = {}
local term_buf = nil
local term_win = nil

function M.toggle()
    if term_win and vim.api.nvim_win_is_valid(term_win) then
        vim.api.nvim_win_close(term_win, true)
        term_win = nil
        term_buf = nil
        return
    end

    if not term_buf or not vim.api.nvim_buf_is_valid(term_buf) then
        term_buf = vim.api.nvim_create_buf(false, true)
    end

    local width = math.floor(vim.o.columns * 0.7)
    local height = math.floor(vim.o.lines * 0.7)
    local row = math.floor((vim.o.lines - height) / 2)
    local col = math.floor((vim.o.columns - width) / 2)

    term_win = vim.api.nvim_open_win(term_buf, true, {
        relative = "editor",
        width = width,
        height = height,
        row = row,
        col = col,
        style = "minimal",
        border = "single",
    })
    vim.fn.termopen(vim.o.shell)
    vim.cmd("startinsert")
    vim.api.nvim_buf_set_keymap(term_buf, "t", "<Esc>", "<C-\\><C-n>:FloatTerm<CR>", {noremap=true, silent=true})
end

vim.api.nvim_create_user_command("FloatTerm", function()
    M.toggle()
end, {})

return M
