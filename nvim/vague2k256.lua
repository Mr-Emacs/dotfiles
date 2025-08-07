-- vague2k256.lua
-- 256-color compatible version of the Vague colorscheme
-- Original color values from VagueColorscheme
local colors = {
	bg          = "#141415", -- ~ 234
	fg          = "#cdcdcd", -- ~ 251
	floatBorder = "#878787", -- ~ 243
	line        = "#252530", -- ~ 235
	comment     = "#606079", -- ~ 60
	builtin     = "#b4d4cf", -- ~ 152
	func        = "#c48282", -- ~ 174
	string      = "#e8b589", -- ~ 216
	number      = "#e0a363", -- ~ 215
	property    = "#c3c3d5", -- ~ 189
	constant    = "#aeaed1", -- ~ 146
	parameter   = "#bb9dbd", -- ~ 146
	visual      = "#333738", -- ~ 236
	error       = "#d8647e", -- ~ 168
	warning     = "#f3be7c", -- ~ 222
	hint        = "#7e98e8", -- ~ 111
	operator    = "#90a0b5", -- ~ 110
	keyword     = "#6e94b2", -- ~ 67
	type        = "#9bb4bc", -- ~ 152
	search      = "#405065", -- ~ 239
	plus        = "#7fa563", -- ~ 107
	delta       = "#f3be7c", -- ~ 222
}

local function hi(group, tbl)
	vim.api.nvim_set_hl(0, group, tbl)
end

-- Clear and set colorscheme name
vim.cmd("hi clear")
if vim.fn.exists("syntax_on") == 1 then vim.cmd("syntax reset") end
vim.g.colors_name = "vague2k256"

-- UI Elements
hi("Normal", { ctermfg = 251, ctermbg = 16 })
hi("LineNr", { ctermfg = 243, ctermbg = 16 })
hi("CursorLine", { ctermbg = 235 })
hi("CursorLineNr", { ctermfg = 222, bold = true })
hi("Visual", { ctermbg = 236 })
hi("StatusLine", { ctermfg = 251, ctermbg = 235 })
hi("StatusLineNC", { ctermfg = 243, ctermbg = 234 })
hi("Pmenu", { ctermfg = 251, ctermbg = 235 })
hi("PmenuSel", { ctermfg = 234, ctermbg = 222 })
hi("PmenuSbar", { ctermbg = 236 })
hi("PmenuThumb", { ctermbg = 222 })
hi("Search", { ctermfg = 234, ctermbg = 239 })
hi("IncSearch", { ctermfg = 234, ctermbg = 222 })
hi("MatchParen", { ctermfg = 168, bold = true })

-- Diagnostics
hi("Error", { ctermfg = 168, bold = true })
hi("WarningMsg", { ctermfg = 222, bold = true })
hi("Hint", { ctermfg = 111 })
hi("Todo", { ctermfg = 215, ctermbg = 234, bold = true })

-- Syntax
hi("Comment", { ctermfg = 60, italic = true })
hi("Constant", { ctermfg = 146 })
hi("String", { ctermfg = 216, italic = true })
hi("Character", { ctermfg = 216 })
hi("Number", { ctermfg = 215 })
hi("Boolean", { ctermfg = 215, bold = true })
hi("Float", { ctermfg = 215 })
hi("Identifier", { ctermfg = 251 })
hi("Function", { ctermfg = 174 })
hi("Statement", { ctermfg = 67, bold = true })
hi("Conditional", { ctermfg = 67 })
hi("Repeat", { ctermfg = 67 })
hi("Label", { ctermfg = 67 })
hi("Operator", { ctermfg = 110 })
hi("Keyword", { ctermfg = 67 })
hi("Type", { ctermfg = 152, bold = true })
hi("StorageClass", { ctermfg = 152 })
hi("Structure", { ctermfg = 152 })
hi("Typedef", { ctermfg = 152 })
hi("PreProc", { ctermfg = 152 })
hi("Include", { ctermfg = 152 })
hi("Define", { ctermfg = 152 })
hi("Macro", { ctermfg = 152 })
hi("Special", { ctermfg = 174 })
hi("SpecialChar", { ctermfg = 174 })
hi("Tag", { ctermfg = 174 })
hi("Delimiter", { ctermfg = 243 })
hi("SpecialComment", { ctermfg = 60 })
hi("Underlined", { ctermfg = 110, underline = true })

-- LSP Diagnostics
hi("DiagnosticError", { ctermfg = 168 })
hi("DiagnosticWarn", { ctermfg = 222 })
hi("DiagnosticHint", { ctermfg = 111 })
hi("DiagnosticInfo", { ctermfg = 110 })

-- Git Signs
hi("GitGutterAdd", { ctermfg = 107 })
hi("GitGutterChange", { ctermfg = 222 })
hi("GitGutterDelete", { ctermfg = 168 })

-- Telescope
hi("TelescopeBorder", { ctermfg = 243 })
hi("TelescopePromptBorder", { ctermfg = 222 })
hi("TelescopeSelection", { ctermbg = 236, bold = true })
hi("TelescopeMatching", { ctermfg = 216, bold = true })
-- Add these to your colorscheme setup
hi("StatusLine", { ctermfg = 251, ctermbg = "NONE" })
hi("StatusLineNC", { ctermfg = 243, ctermbg = "NONE" })

