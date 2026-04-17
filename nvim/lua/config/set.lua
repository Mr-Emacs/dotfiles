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
