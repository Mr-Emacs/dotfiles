" ~/.vimrc

" --- Plugin manager: vim-plug ---
call plug#begin('~/.vim/plugged')

Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'   " Optional, for extra server configs (not required)

call plug#end()

" --- Path to your custom LSP server executable ---
let g:custom_lsp_path = expand('~/programming/vlogc-lsp/main')

" --- Register your custom LSP server ---
au User lsp_setup call lsp#register_server({
      \ 'name': 'vlogcc',
      \ 'cmd': {server_info -> [g:custom_lsp_path, '--stdio']},
      \ 'whitelist': ['vlogcc'],
      \ })

" --- Associate .code extension with vlogcc filetype ---
au BufRead,BufNewFile *.code set filetype=vlogcc

" --- Automatically enable LSP for your custom language ---
autocmd FileType vlogcc call lsp#enable()

" --- Diagnostics signs in gutter ---
sign define LspError text=✗ texthl=LspErrorSign
sign define LspWarning text=! texthl=LspWarningSign
sign define LspInformation text=i texthl=LspInfoSign
sign define LspHint text=? texthl=LspHintSign

" --- Key mappings for LSP functionality ---
nnoremap <silent> gd :LspDefinition<CR>
nnoremap <silent> gr :LspReferences<CR>
nnoremap <silent> K :LspHover<CR>
nnoremap <silent> <leader>rn :LspRename<CR>
nnoremap <silent> <leader>ca :LspCodeAction<CR>

" --- Show diagnostics automatically on CursorHold ---
autocmd CursorHold *.code :LspDocumentDiagnostics

" --- Enable completion and hover ---
let g:lsp_document_highlight_enabled = 1
let g:lsp_text_document_diagnostics_enabled = 1
let g:lsp_completion_enabled = 1
let g:lsp_hover_enabled = 1

" --- LSP settings ---
let g:lsp_log_verbose = 1
let g:lsp_log_file = expand('~/vim-lsp.log')
let g:lsp_diagnostics_enabled = 1
let g:lsp_diagnostics_echo_cursor = 1

" --- Basic editor settings ---
set number
set cursorline
syntax on
filetype plugin indent on

" --- Message on start ---
echo "Custom language LSP loaded for .code files"

