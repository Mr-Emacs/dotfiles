set background=dark
highlight clear
if exists("syntax_on")
      syntax reset
endif
let g:colors_name = "hmh"

hi Normal          guifg=#DAB98F  guibg=#161616  gui=NONE  ctermfg=180  ctermbg=234  cterm=NONE
hi NonText         guifg=#444444  guibg=#161616  gui=NONE  ctermfg=238  ctermbg=234  cterm=NONE
hi LineNr          guifg=#555555  guibg=#161616  gui=NONE  ctermfg=240  ctermbg=234  cterm=NONE
hi CursorLine      guifg=NONE     guibg=#00008B  gui=NONE  ctermfg=NONE ctermbg=18   cterm=NONE
hi CursorLineNr    guifg=#555555  guibg=#161616  gui=NONE  ctermfg=240  ctermbg=234  cterm=NONE
hi CursorColumn    guifg=NONE     guibg=#00008B  gui=NONE  ctermfg=NONE ctermbg=18   cterm=NONE
hi Cursor          guifg=#161616  guibg=#40FF40  gui=NONE  ctermfg=234  ctermbg=82   cterm=NONE
hi iCursor         guifg=#161616  guibg=#40FF40  gui=NONE  ctermfg=234  ctermbg=82   cterm=NONE
hi Visual          guifg=NONE     guibg=#00009F  gui=NONE  ctermfg=NONE ctermbg=19   cterm=NONE
hi VisualNOS       guifg=NONE     guibg=#00009F  gui=NONE  ctermfg=NONE ctermbg=19   cterm=NONE
hi Search          guifg=#161616  guibg=#CD950C  gui=NONE  ctermfg=234  ctermbg=136  cterm=NONE
hi IncSearch       guifg=#161616  guibg=#40FF40  gui=NONE  ctermfg=234  ctermbg=82   cterm=NONE
hi MatchParen      guifg=NONE     guibg=#3A5A8A  gui=NONE  ctermfg=NONE ctermbg=25   cterm=NONE
hi ColorColumn     guifg=NONE     guibg=#2A2A2A  gui=NONE  ctermfg=NONE ctermbg=235  cterm=NONE
hi Folded          guifg=#808080  guibg=#1E1E1E  gui=NONE  ctermfg=244  ctermbg=234  cterm=NONE
hi FoldColumn      guifg=#808080  guibg=#161616  gui=NONE  ctermfg=244  ctermbg=234  cterm=NONE
hi SignColumn      guifg=#808080  guibg=#161616  gui=NONE  ctermfg=244  ctermbg=234  cterm=NONE
hi VertSplit       guifg=#333333  guibg=#161616  gui=NONE  ctermfg=236  ctermbg=234  cterm=NONE
hi StatusLine      guifg=#DAB98F  guibg=#2A2A2A  gui=NONE  ctermfg=180  ctermbg=235  cterm=NONE
hi StatusLineNC    guifg=#555555  guibg=#1E1E1E  gui=NONE  ctermfg=240  ctermbg=234  cterm=NONE
hi WildMenu        guifg=#161616  guibg=#CD950C  gui=NONE  ctermfg=234  ctermbg=136  cterm=NONE
hi Pmenu           guifg=#DAB98F  guibg=#1E1E1E  gui=NONE  ctermfg=180  ctermbg=234  cterm=NONE
hi PmenuSel        guifg=#161616  guibg=#CD950C  gui=NONE  ctermfg=234  ctermbg=136  cterm=NONE
hi PmenuSbar       guifg=NONE     guibg=#2A2A2A  gui=NONE  ctermfg=NONE ctermbg=235  cterm=NONE
hi PmenuThumb      guifg=NONE     guibg=#555555  gui=NONE  ctermfg=NONE ctermbg=240  cterm=NONE
hi TabLine         guifg=#808080  guibg=#1E1E1E  gui=NONE  ctermfg=244  ctermbg=234  cterm=NONE
hi TabLineSel      guifg=#DAB98F  guibg=#161616  gui=NONE  ctermfg=180  ctermbg=234  cterm=NONE
hi TabLineFill     guifg=NONE     guibg=#1E1E1E  gui=NONE  ctermfg=NONE ctermbg=234  cterm=NONE
hi Title           guifg=#DAB98F  guibg=NONE     gui=bold  ctermfg=180  ctermbg=NONE cterm=bold
hi EndOfBuffer     guifg=#333333  guibg=NONE     gui=NONE  ctermfg=236  ctermbg=NONE cterm=NONE
hi ModeMsg         guifg=#DAB98F  guibg=NONE     gui=NONE  ctermfg=180  ctermbg=NONE cterm=NONE
hi MoreMsg         guifg=#6B8E23  guibg=NONE     gui=NONE  ctermfg=64   ctermbg=NONE cterm=NONE
hi Question        guifg=#6B8E23  guibg=NONE     gui=NONE  ctermfg=64   ctermbg=NONE cterm=NONE
hi ErrorMsg        guifg=#FF0000  guibg=NONE     gui=bold  ctermfg=196  ctermbg=NONE cterm=bold
hi WarningMsg      guifg=#CD950C  guibg=NONE     gui=NONE  ctermfg=136  ctermbg=NONE cterm=NONE
hi Directory       guifg=#CD950C  guibg=NONE     gui=NONE  ctermfg=136  ctermbg=NONE cterm=NONE
hi SpecialKey      guifg=#444444  guibg=NONE     gui=NONE  ctermfg=238  ctermbg=NONE cterm=NONE
hi Whitespace      guifg=#444444  guibg=NONE     gui=NONE  ctermfg=238  ctermbg=NONE cterm=NONE
hi ExtraWhitespace guifg=NONE     guibg=#2A2A2A  gui=NONE  ctermfg=NONE ctermbg=235  cterm=NONE
hi DiffAdd         guifg=#6B8E23  guibg=#1A2A1A  gui=NONE  ctermfg=64   ctermbg=22   cterm=NONE
hi DiffDelete      guifg=#FF0000  guibg=#2A1A1A  gui=NONE  ctermfg=196  ctermbg=52   cterm=NONE
hi DiffChange      guifg=#DAB98F  guibg=#1A1A2A  gui=NONE  ctermfg=180  ctermbg=17   cterm=NONE
hi DiffText        guifg=#CD950C  guibg=#1A1A2A  gui=bold  ctermfg=136  ctermbg=17   cterm=bold
hi Comment         guifg=#808080  guibg=NONE     gui=NONE  ctermfg=244  ctermbg=NONE cterm=NONE
hi String          guifg=#6B8E23  guibg=NONE     gui=NONE  ctermfg=64   ctermbg=NONE cterm=NONE
hi Constant        guifg=#6B8E23  guibg=NONE     gui=NONE  ctermfg=64   ctermbg=NONE cterm=NONE
hi Character       guifg=#6B8E23  guibg=NONE     gui=NONE  ctermfg=64   ctermbg=NONE cterm=NONE
hi Number          guifg=#DAB98F  guibg=NONE     gui=NONE  ctermfg=180  ctermbg=NONE cterm=NONE
hi Boolean         guifg=#6B8E23  guibg=NONE     gui=NONE  ctermfg=64   ctermbg=NONE cterm=NONE
hi Float           guifg=#DAB98F  guibg=NONE     gui=NONE  ctermfg=180  ctermbg=NONE cterm=NONE
hi Statement       guifg=#CD950C  guibg=NONE     gui=NONE  ctermfg=136  ctermbg=NONE cterm=NONE
hi Keyword         guifg=#CD950C  guibg=NONE     gui=NONE  ctermfg=136  ctermbg=NONE cterm=NONE
hi Conditional     guifg=#CD950C  guibg=NONE     gui=NONE  ctermfg=136  ctermbg=NONE cterm=NONE
hi Repeat          guifg=#CD950C  guibg=NONE     gui=NONE  ctermfg=136  ctermbg=NONE cterm=NONE
hi Label           guifg=#CD950C  guibg=NONE     gui=NONE  ctermfg=136  ctermbg=NONE cterm=NONE
hi Operator        guifg=#CD950C  guibg=NONE     gui=NONE  ctermfg=136  ctermbg=NONE cterm=NONE
hi Exception       guifg=#CD950C  guibg=NONE     gui=NONE  ctermfg=136  ctermbg=NONE cterm=NONE
hi Special         guifg=#DAB98F  guibg=NONE     gui=NONE  ctermfg=180  ctermbg=NONE cterm=NONE
hi SpecialChar     guifg=#6B8E23  guibg=NONE     gui=NONE  ctermfg=64   ctermbg=NONE cterm=NONE
hi Tag             guifg=#DAB98F  guibg=NONE     gui=NONE  ctermfg=180  ctermbg=NONE cterm=NONE
hi Delimiter       guifg=#DAB98F  guibg=NONE     gui=NONE  ctermfg=180  ctermbg=NONE cterm=NONE
hi SpecialComment  guifg=#DAB98F  guibg=NONE     gui=NONE  ctermfg=180  ctermbg=NONE cterm=NONE
hi Debug           guifg=#DAB98F  guibg=NONE     gui=NONE  ctermfg=180  ctermbg=NONE cterm=NONE
hi Function        guifg=#CDAA7D  guibg=NONE     gui=NONE  ctermfg=180  ctermbg=NONE cterm=NONE
hi Identifier      guifg=#CDAA7D  guibg=NONE     gui=NONE  ctermfg=180  ctermbg=NONE cterm=NONE
hi Type            guifg=#CDAA7D  guibg=NONE     gui=NONE  ctermfg=180  ctermbg=NONE cterm=NONE
hi StorageClass    guifg=#CD950C  guibg=NONE     gui=NONE  ctermfg=136  ctermbg=NONE cterm=NONE
hi Structure       guifg=#CD950C  guibg=NONE     gui=NONE  ctermfg=136  ctermbg=NONE cterm=NONE
hi Typedef         guifg=#CDAA7D  guibg=NONE     gui=NONE  ctermfg=180  ctermbg=NONE cterm=NONE
hi PreProc         guifg=#DAB98F  guibg=NONE     gui=NONE  ctermfg=180  ctermbg=NONE cterm=NONE
hi Include         guifg=#DAB98F  guibg=NONE     gui=NONE  ctermfg=180  ctermbg=NONE cterm=NONE
hi Define          guifg=#DAB98F  guibg=NONE     gui=NONE  ctermfg=180  ctermbg=NONE cterm=NONE
hi Macro           guifg=#DAB98F  guibg=NONE     gui=NONE  ctermfg=180  ctermbg=NONE cterm=NONE
hi PreCondit       guifg=#DAB98F  guibg=NONE     gui=NONE  ctermfg=180  ctermbg=NONE cterm=NONE
hi Underlined      guifg=#DAB98F  guibg=NONE     gui=underline ctermfg=180 ctermbg=NONE cterm=underline
hi Ignore          guifg=#444444  guibg=NONE     gui=NONE  ctermfg=238  ctermbg=NONE cterm=NONE
hi Error           guifg=#FF0000  guibg=NONE     gui=bold  ctermfg=196  ctermbg=NONE cterm=bold
hi Todo            guifg=#FF0000  guibg=NONE     gui=bold,italic ctermfg=196 ctermbg=NONE cterm=bold
hi HmhNote         guifg=#006400  guibg=NONE     gui=bold,italic ctermfg=22  ctermbg=NONE cterm=bold
hi SpellBad        guifg=NONE     guibg=NONE     gui=undercurl guisp=#FF0000 ctermfg=NONE ctermbg=NONE cterm=underline
hi SpellCap        guifg=NONE     guibg=NONE     gui=undercurl guisp=#CD950C ctermfg=NONE ctermbg=NONE cterm=underline
hi SpellRare       guifg=NONE     guibg=NONE     gui=undercurl guisp=#6B8E23 ctermfg=NONE ctermbg=NONE cterm=underline
hi SpellLocal      guifg=NONE     guibg=NONE     gui=undercurl guisp=#808080 ctermfg=NONE ctermbg=NONE cterm=underline

augroup hmh_note_keyword
      autocmd!
        autocmd Syntax * syn keyword HmhNote NOTE containedin=.*Comment.*
          autocmd Syntax * hi def link HmhNote HmhNote
augroup END
