"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugin configuration
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""
" => Load vundle
""""""""""""""""""""""""""""""

set rtp+=~/.vim/bundle/Vundle.vim
let s:vim_runtime = expand('<sfile>:p:h')."/.."

filetype off
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'https://github.com/google/vim-glaive'
Plugin 'https://github.com/google/vim-maktaba'
Plugin 'https://github.com/embear/vim-localvimrc'
Plugin 'https://github.com/xolox/vim-misc'
Plugin 'https://github.com/xolox/vim-colorscheme-switcher'
Plugin 'https://github.com/tomtom/tlib_vim'
Plugin 'https://github.com/sophacles/vim-bundle-mako'
Plugin 'https://github.com/tpope/vim-repeat'
Plugin 'https://github.com/MarcWeber/vim-addon-mw-utils'
Plugin 'https://github.com/scrooloose/nerdtree'
Plugin 'https://github.com/Xuyuanp/nerdtree-git-plugin'
Plugin 'https://github.com/rafi/awesome-vim-colorschemes'
Plugin 'https://github.com/itchyny/lightline.vim'
Plugin 'https://github.com/bronson/vim-trailing-whitespace'
Plugin 'https://github.com/google/vim-codefmt'
Plugin 'https://github.com/tpope/vim-surround'
Plugin 'https://github.com/tpope/vim-commentary'
Plugin 'https://github.com/godlygeek/tabular'
Plugin 'https://github.com/maxbrunsfeld/vim-yankstack'
Plugin 'https://github.com/michaeljsmith/vim-indent-object'
Plugin 'https://github.com/terryma/vim-expand-region'
Plugin 'https://github.com/chiedo/vim-case-convert'
set rtp+=/usr/local/opt/fzf
Plugin 'https://github.com/junegunn/fzf.vim'
Plugin 'https://github.com/mileszs/ack.vim'
Plugin 'https://github.com/kien/ctrlp.vim'
Plugin 'https://github.com/jlanzarotta/bufexplorer'
Plugin 'https://github.com/2072/PHP-Indenting-for-VIm'
Plugin 'https://github.com/mxw/vim-jsx'
Plugin 'https://github.com/StanAngeloff/php.vim'
Plugin 'https://github.com/fatih/vim-go'
Plugin 'https://github.com/groenewege/vim-less'
Plugin 'https://github.com/chr4/nginx.vim'
Plugin 'https://github.com/plasticboy/vim-markdown'
Plugin 'https://github.com/airblade/vim-gitgutter'
Plugin 'https://github.com/tpope/vim-fugitive'
Plugin 'https://github.com/Konfekt/vim-scratchpad'
Plugin 'https://github.com/vim-scripts/ZoomWin'
Plugin 'mattn/emmet-vim'
Plugin 'https://github.com/majutsushi/tagbar'
Plugin 'andymass/vim-matchup'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'keith/swift.vim'

call vundle#end()
filetype plugin indent on

""""""""""""""""""""""""""""""
" => bufExplorer plugin
""""""""""""""""""""""""""""""

let g:bufExplorerDefaultHelp=0
let g:bufExplorerShowRelativePath=1
let g:bufExplorerFindActive=1
let g:bufExplorerSortBy='mru'
map <leader>o :BufExplorer<cr>

""""""""""""""""""""""""""""""
" => YankStack
""""""""""""""""""""""""""""""

let g:yankstack_yank_keys = ['y', 'd']

nmap <c-p> <Plug>yankstack_substitute_older_paste
nmap <c-n> <Plug>yankstack_substitute_newer_paste

""""""""""""""""""""""""""""""
" => CTRL-P
""""""""""""""""""""""""""""""

let g:ctrlp_working_path_mode = 'r'
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git'
let g:ctrlp_max_height = 20

map <leader>j :CtrlP<cr>

" CTags navigation
nnoremap <leader>t :CtrlPTag<cr>

""""""""""""""""""""""""""""""
" => Vim grep
""""""""""""""""""""""""""""""

let Grep_Skip_Dirs = 'RCS CVS SCCS .svn generated'
set grepprg=/bin/grep\ -nH

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Nerd Tree
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:NERDTreeWinPos = "left"
let NERDTreeShowHidden=1
let NERDTreeIgnore = ['\.pyc$', '__pycache__', '\.o$', '\.d$']
let g:NERDTreeWinSize=40
let g:NERDTreeHighlightCursorline=0

" Auto-remove buffer of the file when it's deleted from the NERDTREE
let NERDTreeAutoDeleteBuffer = 1

" Automatically close NerdTree when it's the only buffer left
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Keybindings
map <leader>nn :NERDTreeToggle<cr>
map <leader>nb :NERDTreeFromBookmark<Space>
map <leader>nf :NERDTreeFind<cr>

" Appearance settings
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1

" Open NERDTree on Vim launch
" autocmd StdinReadPre * let s:std_in=1
" autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

" Automatically close Vim if there is only NERD tree window left
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => surround.vim config
" Annotate strings with gettext
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

vmap Si S(i_<esc>f)
au FileType mako vmap Si S"i${ _(<esc>2f"a) }<esc>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => lightline
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ }

let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'active': {
      \   'left': [ ['mode', 'paste'],
      \             ['fugitive', 'readonly', 'filename', 'modified'] ],
      \   'right': [ [ 'lineinfo' ], ['percent'] ]
      \ },
      \ 'component': {
      \   'readonly': '%{&filetype=="help"?"":&readonly?"🔒":""}',
      \   'modified': '%{&filetype=="help"?"":&modified?"+":&modifiable?"":"-"}',
      \   'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
      \ },
      \ 'component_visible_condition': {
      \   'readonly': '(&filetype!="help"&& &readonly)',
      \   'modified': '(&filetype!="help"&&(&modified||!&modifiable))',
      \   'fugitive': '(exists("*fugitive#head") && ""!=fugitive#head())'
      \ },
      \ 'separator': { 'left': ' ', 'right': ' ' },
      \ 'subseparator': { 'left': ' ', 'right': ' ' }
      \ }

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Vim-go
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:go_fmt_command = "goimports"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Git gutter (Git diff)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:gitgutter_enabled=0
nnoremap <silent> <leader>d :GitGutterToggle<cr>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => CodeFMT
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

autocmd BufNewFile,BufRead *.jsx set syntax=javascript

autocmd FileType c,cpp,objc nnoremap <buffer><Leader>cf :<C-u>FormatCode clang-format<CR>
autocmd FileType python nnoremap <buffer><Leader>cf :<C-u>FormatCode yapf<CR>
autocmd FileType javascript,css,scss,less,html nnoremap <buffer><Leader>cf :<C-u>FormatCode prettier<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => RootIgnore
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:RootIgnoreUseHome = 1
let g:RootIgnoreAgignore = 0

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => TrailingWhitespace
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

autocmd BufWritePost * FixWhitespace

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => LocalVimrc
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:localvimrc_ask = 0

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => AutoPair
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let b:autopairs_enabled = 0

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Scratchpad
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:scratchpad_path = '.scratchpads'
let g:scratchpad_ftype = ''
nmap <leader>s <Plug>(ToggleScratchPad)

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Next color scheme
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nmap <leader>tr :RandomColorScheme<cr>
nmap <leader>tn :NextColorScheme<cr>
nmap <leader>tp :PrevColorScheme<cr>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Tagbar
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nmap <leader>b :TagbarToggle<CR>
