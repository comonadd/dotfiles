" leader key
let mapleader = " "

" Concealing
set conceallevel=1

" Disable scrollbars
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L

" Indentation settings
set smarttab
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2

" Automatically write before executing commands
set autowrite

" Enable incremental search
set incsearch

" Show incompleted commands
set showcmd

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Turn persistent undo on
"    means that you can undo even when you close a buffer/VIM
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
try
  set undodir=~/.vim_runtime/temp_dirs/undodir
  set undofile
catch
endtry

" Line numbers
set number
set relativenumber

" Enable the highlight of the current line
set cursorline

" Display the whitespace
set list
set listchars=tab:>-,trail:~
set backspace=indent,eol,start
