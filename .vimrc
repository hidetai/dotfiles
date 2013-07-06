set nocompatible

" Configuration for NeoBundle
if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif
call neobundle#rc(expand('~/.vim/bundle'))

" Let NeoBundle manage NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'

""" Bundles
" Unite
NeoBundle 'Shougo/unite.vim'

" Molokai Color Scheme
NeoBundle 'tomasr/molokai'

syntax on

filetype plugin on
filetype indent on

colorscheme molokai

" Highlighten
set hlsearch
