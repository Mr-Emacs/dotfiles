local colors = {
	bg           = "#000000",
	fg           = "#ffffff",
	red          = "#FF5E40",
	orange       = "#F6CA83",
	black        = "#181825",
	green        = "#a6e3a1",
	yellow       = "#FCDC4D",
	blue         = "#89b4fa",
	magenta      = "#f5c2e7",
	cyan         = "#94e2d5",
	grey        = "#bac2de",
	gray         = "#45475a",
	darkgray     = "#313244",
	lightgray    = "#040F0F",
}

vim.opt.termguicolors = true

vim.cmd("highlight clear")
vim.cmd("syntax reset")
vim.g.colors_name = "grub-dark"

-- Highlight groups
local function hi(group, opts)
	vim.api.nvim_set_hl(0, group, opts)
end

-- UI
hi("Normal", { fg = colors.fg, bg = colors.bg })
hi("CursorLine", { bg = colors.darkgray })
hi("CursorColumn", { bg = colors.darkgray })
hi("Visual", { bg = colors.gray })
hi("LineNr", { fg = colors.gray, bg = colors.bg })
hi("CursorLineNr", { fg = colors.yellow, bold = false })
hi("VertSplit", { fg = colors.darkgray })
hi("StatusLine", { fg = colors.fg, bg = colors.darkgray })
hi("StatusLineNC", { fg = colors.lightgray, bg = colors.darkgray })
hi("Pmenu", { fg = colors.fg, bg = colors.black })
hi("PmenuSel", { fg = colors.black, bg = colors.blue })

-- Syntax
hi("Comment", { fg = colors.lightgray, italic = true })
hi("String", { fg = colors.green })
hi("Function", { fg = colors.blue })
hi("Identifier", { fg = colors.magenta })
hi("Keyword", { fg = colors.red, bold = true })
hi("Type", { fg = colors.yellow })
hi("Constant", { fg = colors.cyan })
hi("Number", { fg = colors.cyan })
hi("Boolean", { fg = colors.cyan })
hi("Operator", { fg = colors.fg })
hi("Statement", { fg = colors.red })

-- Diffs
hi("DiffAdd", { bg = "#27352f" })
hi("DiffChange", { bg = "#27394e" })
hi("DiffDelete", { bg = "#3f2d3d" })
hi("DiffText", { bg = "#394b70" })

-- Diagnostics
hi("DiagnosticError", { fg = colors.red })
hi("DiagnosticWarn", { fg = colors.yellow })
hi("DiagnosticInfo", { fg = colors.blue })
hi("DiagnosticHint", { fg = colors.cyan })

-- GitSigns
hi("GitSignsAdd", { fg = colors.green })
hi("GitSignsChange", { fg = colors.yellow })
hi("GitSignsDelete", { fg = colors.red })

-- Tabs
hi("TabLine", { fg = colors.lightgray, bg = colors.darkgray })
hi("TabLineFill", { bg = colors.darkgray })
hi("TabLineSel", { fg = colors.fg, bg = colors.bg })

-- Search
hi("Search", { bg = colors.yellow, fg = colors.black })
hi("IncSearch", { bg = colors.magenta, fg = colors.black })

-- Misc
hi("Title", { fg = colors.blue, bold = true })
hi("ErrorMsg", { fg = colors.red, bold = true })
hi("WarningMsg", { fg = colors.yellow })
hi("Todo", { fg = colors.magenta, bold = true })

-- Treesitter (optional)
hi("@variable", { fg = colors.fg })
hi("@function", { fg = colors.blue })
hi("@keyword", { fg = colors.red })
hi("@type", { fg = colors.yellow })
hi("@comment", { fg = colors.grey, italic = false })
