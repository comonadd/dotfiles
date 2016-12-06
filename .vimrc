set nocompatible
filetype off

" Vundle Initialization BEG
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
    Plugin 'VundleVim/Vundle.vim'
    Plugin 'scrooloose/nerdtree'
call vundle#end()
filetype plugin indent on
" Vundle Initialization END

"Setting colorscheme BEG
color desert
"Setting colorscheme END

" Setting flag variables BEG
set smarttab
set expandtab
set number
set cursorline
set cursorline
set list
" Setting flag variables END

" Setting value variables BEG
set tabstop=4
set softtabstop=4
set shiftwidth=4
set listchars=tab:>-,trail:~
set backspace=indent,eol,start
" Setting value variables END

" Keys mapping BEG
"" Navigation BEG
"" Navigation END

"" Editing BEG
vnoremap <C-c> "*y

inoremap <C-v> <Esc>"*pa
nnoremap <C-v> "*p

inoremap <C-x> <Esc>"*dda
nnoremap <C-x> "*dd
vnoremap <C-x> "*d
"" Editing END
" Keys mapping END

" Main stuff BEG
hi cursorline term=bold cterm=bold guibg=black
autocmd BufWritePre *.* :%s/\s\+$//e
" Main stuff END
