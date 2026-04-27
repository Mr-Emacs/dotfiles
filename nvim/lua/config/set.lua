local home = vim.fn.expand("~")
local is_windows = vim.fn.has("win32") == 1 or vim.fn.has("win64") == 1

local programming_dir
if is_windows then
  programming_dir = "C:/Programming"
else
  programming_dir = home .. "/Programming"
end

if vim.fn.isdirectory(programming_dir) == 1 then
  vim.cmd("cd " .. programming_dir)
else
  vim.notify("Directory not found: " .. programming_dir, vim.log.levels.WARN)
end

local config_dir = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":h")
vim.cmd("source " .. config_dir .. "/hmh.vim")

vim.api.nvim_set_hl(0, "@variable",          { fg = "#DAB98F" })
vim.api.nvim_set_hl(0, "@variable.builtin",  { fg = "#DAB98F" })
vim.api.nvim_set_hl(0, "@variable.member",   { fg = "#DAB98F" })
vim.api.nvim_set_hl(0, "@variable.parameter",{ fg = "#DAB98F" })
vim.api.nvim_set_hl(0, "@property",          { fg = "#DAB98F" })
vim.api.nvim_set_hl(0, "@field",             { fg = "#DAB98F" })
vim.api.nvim_set_hl(0, "@lsp.type.operator",   { fg = "#DAB98F" })
vim.api.nvim_set_hl(0, "@lsp.type.operator.c", { fg = "#DAB98F" })
