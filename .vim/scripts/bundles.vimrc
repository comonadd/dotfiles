" Prepare for the Vundle initialization
set nocompatible
filetype off

" Initialize Vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Feature-provide plugins
Plugin 'VundleVim/Vundle.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'scrooloose/nerdcommenter'
Plugin 'drmingdrmer/vim-indent-lua'
Plugin 'leafgarland/typescript-vim'
Plugin 'tpope/vim-surround'
Plugin 'elzr/vim-json'
Plugin 'tpope/vim-fugitive'
Plugin 'kien/ctrlp.vim'
Plugin 'bling/vim-airline'
Plugin 'tpope/vim-repeat'
Plugin 'pangloss/vim-javascript'
Plugin 'easymotion/vim-easymotion'
Plugin 'godlygeek/tabular'
Plugin 'mxw/vim-jsx'
Plugin 'jparise/vim-graphql'
Plugin 'ianks/vim-tsx'
Plugin 'jreybert/vimagit'
Plugin 'junegunn/fzf'
Plugin 'junegunn/fzf.vim'

" Colorscheme plugins
Plugin 'flazz/vim-colorschemes'
Plugin 'anoike/vim-moneyforward'
Plugin 'drewtempelmeyer/palenight.vim'
Plugin 'cseelus/vim-colors-clearance'
Plugin 'lu-ren/SerialExperimentsLain'
Plugin 'kocakosm/hilal'
Plugin 'altercation/vim-colors-solarized'
Plugin 'tomasr/molokai'
Plugin 'morhetz/gruvbox'
Plugin 'w0ng/vim-hybrid'

" Complete the Vundle initialization
call vundle#end()
filetype plugin indent on
