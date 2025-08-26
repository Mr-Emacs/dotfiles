set nocompatible
filetype off
syntax on
set nu
set nobackup
set nowritebackup
set noswapfile

let s:data_dir = expand('~/.vim')

if !isdirectory(s:data_dir . '/autoload')
  call mkdir(s:data_dir . '/autoload', 'p', 0700)
endif

if empty(glob(s:data_dir . '/autoload/plug.vim'))
  echo "Installing vim-plug..."
  let s:plug_path = s:data_dir . '/autoload/plug.vim'
  let s:curl_cmd = 'curl -fLo ' . shellescape(s:plug_path) . ' --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  silent execute '!' . s:curl_cmd
  if filereadable(s:plug_path)
    augroup vim_plug_install
      autocmd!
      autocmd VimEnter * ++once PlugInstall --sync | source $MYVIMRC
    augroup END
  else
    echoerr "Failed to download vim-plug. Please check curl and network."
  endif
endif

if filereadable(s:data_dir . '/autoload/plug.vim')
  execute 'runtime autoload/plug.vim'
endif

call plug#begin('~/.vim/plugged')

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'xsoder/gruber-darker'
Plug 'vim-airline/vim-airline'

call plug#end()

colorscheme gruber-darker
