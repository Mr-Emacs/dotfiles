" Name: vague2k256
" Description: 256-color compatible version of the Vague colorscheme
" Maintainer: You
" Version: 1.0

hi clear
if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "vague2k256"

" UI Elements
hi Normal         ctermfg=251 ctermbg=16
hi LineNr         ctermfg=243 ctermbg=16
hi CursorLine                 ctermbg=235
hi CursorLineNr   ctermfg=222           
hi Visual                     ctermbg=243
hi StatusLine     ctermfg=251 ctermbg=235
hi StatusLineNC   ctermfg=243 ctermbg=234
hi Pmenu          ctermfg=251 ctermbg=235
hi PmenuSel       ctermfg=234 ctermbg=222
hi PmenuSbar                  ctermbg=236
hi PmenuThumb                 ctermbg=222
hi Search         ctermfg=234 ctermbg=239
hi IncSearch      ctermfg=234 ctermbg=222
hi MatchParen     ctermfg=168           cterm=bold

" Diagnostics
hi Error          ctermfg=168           cterm=bold
hi WarningMsg     ctermfg=222           cterm=bold
hi Hint           ctermfg=111
hi Todo           ctermfg=215 ctermbg=234 cterm=bold

" Syntax
hi Comment        ctermfg=60
hi Constant       ctermfg=146
hi String         ctermfg=216
hi Character      ctermfg=216
hi Number         ctermfg=215
hi Boolean        ctermfg=215           cterm=bold
hi Float          ctermfg=215
hi Identifier     ctermfg=251
hi Function       ctermfg=174
hi Statement      ctermfg=67            cterm=bold
hi Conditional    ctermfg=67
hi Repeat         ctermfg=67
hi Label          ctermfg=67
hi Operator       ctermfg=110
hi Keyword        ctermfg=67
hi Type           ctermfg=152           cterm=bold
hi StorageClass   ctermfg=152
hi Structure      ctermfg=152
hi Typedef        ctermfg=152
hi PreProc        ctermfg=152
hi Include        ctermfg=152
hi Define         ctermfg=152
hi Macro          ctermfg=152
hi Special        ctermfg=174
hi SpecialChar    ctermfg=174
hi Tag            ctermfg=174
hi Delimiter      ctermfg=243
hi SpecialComment ctermfg=60
hi Underlined     ctermfg=110           cterm=underline

" LSP Diagnostics
hi DiagnosticError ctermfg=168
hi DiagnosticWarn  ctermfg=222
hi DiagnosticHint  ctermfg=111
hi DiagnosticInfo  ctermfg=110

" Git Signs
hi GitGutterAdd    ctermfg=107
hi GitGutterChange ctermfg=222
hi GitGutterDelete ctermfg=168

" Telescope
hi TelescopeBorder        ctermfg=243
hi TelescopePromptBorder  ctermfg=222
hi TelescopeSelection                ctermbg=236 cterm=bold
hi TelescopeMatching       ctermfg=216           cterm=bold

" StatusLine override (transparent background)
hi StatusLine     ctermfg=251 ctermbg=NONE
hi StatusLineNC   ctermfg=243 ctermbg=NONE

