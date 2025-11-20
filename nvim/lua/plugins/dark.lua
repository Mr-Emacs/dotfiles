vim.cmd('hi clear')
if vim.fn.exists('syntax_on') then
    vim.cmd('syntax reset')
end

vim.g.colors_name = 'dark'
vim.o.background = 'dark'

-- Color palette
local colors = {
    bg        = '#161616',
    fg        = '#ffffff',
    cursor    = '#40FF40',
    hl_line   = '#101040',
    selection = '#3a3a8f',
    bracket   = '#cdaa7d',
    string    = '#6b8e23',
    comment   = '#7f7f7f',
    keyword   = '#cd950c',
    constant  = '#6b8e23',
    invalid   = '#f44747',
    command   = '#61afef',
    purple    = '#654CA8',
    dark_gray = '#404040',
}

-- Helper function to set highlights
local function hl(group, opts)
    vim.api.nvim_set_hl(0, group, opts)
end

-- Basic UI
hl('Normal', { fg = colors.fg, bg = colors.bg })
hl('Cursor', { bg = colors.cursor })
hl('CursorLine', { bg = colors.hl_line })
hl('Visual', { bg = colors.selection })
hl('VisualNOS', { bg = colors.selection })
hl('LineNr', { fg = colors.comment, bg = colors.bg })
hl('CursorLineNr', { fg = colors.keyword, bg = colors.bg, bold = true })
hl('SignColumn', { fg = colors.fg, bg = colors.bg })

-- Search
hl('Search', { fg = colors.fg, bg = colors.purple, bold = true })
hl('IncSearch', { fg = colors.fg, bg = colors.purple, bold = true })
hl('CurSearch', { fg = colors.fg, bg = colors.purple, bold = true })

-- Matching parentheses
hl('MatchParen', { fg = colors.fg, bg = colors.purple, bold = true })

-- Syntax highlighting
hl('Comment', { fg = colors.comment })
hl('Constant', { fg = colors.constant })
hl('String', { fg = colors.string })
hl('Character', { fg = colors.string })
hl('Number', { fg = colors.constant })
hl('Boolean', { fg = colors.constant })
hl('Float', { fg = colors.constant })
hl('Identifier', { fg = colors.fg })
hl('Function', { fg = colors.fg })
hl('Statement', { fg = colors.keyword, bold = true })
hl('Conditional', { fg = colors.keyword, bold = true })
hl('Repeat', { fg = colors.keyword, bold = true })
hl('Label', { fg = colors.keyword, bold = true })
hl('Operator', { fg = colors.keyword, bold = true })
hl('Keyword', { fg = colors.keyword, bold = true })
hl('Exception', { fg = colors.keyword, bold = true })
hl('PreProc', { fg = colors.bracket })
hl('Include', { fg = colors.bracket })
hl('Define', { fg = colors.bracket })
hl('Macro', { fg = colors.bracket })
hl('PreCondit', { fg = colors.bracket })
hl('Type', { fg = colors.bracket })
hl('StorageClass', { fg = colors.bracket })
hl('Structure', { fg = colors.bracket })
hl('Typedef', { fg = colors.bracket })
hl('Special', { fg = colors.bracket })
hl('SpecialChar', { fg = colors.bracket })
hl('Tag', { fg = colors.bracket })
hl('Delimiter', { fg = colors.fg })
hl('SpecialComment', { fg = colors.comment })
hl('Debug', { fg = colors.invalid, bold = true })
hl('Error', { fg = colors.fg, bg = colors.invalid, bold = true })
hl('ErrorMsg', { fg = colors.invalid, bold = true })
hl('Warning', { fg = colors.invalid, bold = true })
hl('WarningMsg', { fg = colors.invalid, bold = true })

-- Completion menu
hl('Pmenu', { fg = colors.fg, bg = colors.hl_line })
hl('PmenuSel', { fg = colors.bg, bg = colors.cursor })
hl('PmenuSbar', { bg = colors.selection })
hl('PmenuThumb', { bg = colors.cursor })

-- Diffs
hl('DiffAdd', { fg = colors.cursor, bold = true })
hl('DiffChange', { fg = colors.keyword })
hl('DiffDelete', { fg = colors.invalid, bold = true })
hl('DiffText', { fg = colors.command, bold = true })

-- Folds
hl('Folded', { fg = colors.comment, bg = colors.bg })
hl('FoldColumn', { fg = colors.comment, bg = colors.bg })

-- Statusline (using default)
-- Commented out to match the Emacs theme which uses default mode-line
-- hl('StatusLine', { fg = colors.fg, bg = colors.bg })
-- hl('StatusLineNC', { fg = colors.comment, bg = colors.bg })

-- Tabs
hl('TabLine', { fg = colors.comment, bg = colors.bg })
hl('TabLineFill', { bg = colors.bg })
hl('TabLineSel', { fg = colors.fg, bg = colors.bg, bold = true })

-- Prompts
hl('Question', { fg = colors.keyword, bold = true })
hl('ModeMsg', { fg = colors.keyword, bold = true })
hl('MoreMsg', { fg = colors.keyword, bold = true })
hl('Title', { fg = colors.cursor, bold = true })

-- Whitespace
hl('Whitespace', { fg = colors.dark_gray })
hl('NonText', { fg = colors.dark_gray })
hl('SpecialKey', { fg = colors.dark_gray })

-- Spelling
hl('SpellBad', { fg = colors.invalid, undercurl = true })
hl('SpellCap', { fg = colors.keyword, undercurl = true })
hl('SpellLocal', { fg = colors.command, undercurl = true })
hl('SpellRare', { fg = colors.purple, undercurl = true })

-- Neovim specific
hl('NormalFloat', { fg = colors.fg, bg = colors.hl_line })
hl('FloatBorder', { fg = colors.comment, bg = colors.hl_line })
hl('Directory', { fg = colors.command, bold = true })

-- Treesitter
hl('@comment', { link = 'Comment' })
hl('@constant', { link = 'Constant' })
hl('@string', { link = 'String' })
hl('@number', { link = 'Number' })
hl('@boolean', { link = 'Boolean' })
hl('@function', { fg = colors.fg })
hl('@function.builtin', { fg = colors.bracket })
hl('@keyword', { link = 'Keyword' })
hl('@type', { link = 'Type' })
hl('@variable', { fg = colors.fg })
hl('@variable.builtin', { fg = colors.bracket })
hl('@property', { fg = colors.fg })

-- LSP
hl('DiagnosticError', { fg = colors.invalid })
hl('DiagnosticWarn', { fg = colors.keyword })
hl('DiagnosticInfo', { fg = colors.command })
hl('DiagnosticHint', { fg = colors.comment })

-- Git signs
hl('GitSignsAdd', { fg = colors.cursor })
hl('GitSignsChange', { fg = colors.keyword })
hl('GitSignsDelete', { fg = colors.invalid })
