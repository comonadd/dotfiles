scriptencoding utf-8

set nocompatible
filetype off

call plug#begin(stdpath('data') . '/plugged')

" Linters, fixers, formatters, etc.
Plug 'w0rp/ale'

" Searcher
Plug 'mileszs/ack.vim'

" Python import sorter
Plug 'brentyi/isort.vim'

" Go plugin (does most things Go-related)
Plug 'fatih/vim-go'

" Fuzzy file finder (like Ctrl+K in other apps)
Plug 'ctrlpvim/ctrlp.vim'

Plug 'editorconfig/editorconfig-vim'

" Allow plugins to define their own operator
Plug 'kana/vim-operator-user'

" clang-format plugin
Plug 'rhysd/vim-clang-format'

" Plug which allows me to press a button to toggle between header and source
" file. Currently bound to LEADER+H
Plug 'ericcurtin/CurtineIncSw.vim'

" Comments
Plug 'tomtom/tcomment_vim'

Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'rust-analyzer/rust-analyzer'

Plug 'leafgarland/typescript-vim'

Plug 'liuchengxu/graphviz.vim'

Plug 'prabirshrestha/async.vim'

Plug 'martinda/Jenkinsfile-vim-syntax'

Plug 'modille/groovy.vim'

Plug 'sk1418/HowMuch'

Plug 'vim-scripts/loremipsum'

" Multiple cursor
Plug 'mg979/vim-visual-multi', {'branch': 'master'}

Plug 'itchyny/lightline.vim'

Plug 'xolox/vim-misc'
Plug 'xolox/vim-colorscheme-switcher'

" Colorschemes
Plug 'fenetikm/falcon'
Plug 'nanotech/jellybeans.vim'
Plug 'tomasr/molokai'

call plug#end()
filetype plugin indent on

syntax on

" Ignore various cache/vendor folders
set wildignore+=*/node_modules/*,*/dist/*,*/__pycache__/*,*/venv/*

" Ignore C/C++ Object files
set wildignore+=*.o,*.obj
set wildignore+=*.ilk
set wildignore+=*/build/*
set wildignore+=*/build_native/*
set wildignore+=*/build-*/*
set wildignore+=*/vendor/*

" Ignore generated C/C++ Qt files
set wildignore+=moc_*.cpp,moc_*.h

" Ignore generated C/C++ Qt files
set wildignore+=moc_*.cpp,moc_*.h
" set wildignore+=*/lib/*
set wildignore+=*/target/debug/*
set wildignore+=*/target/release/*

" Ignore Unity asset meta-files
set wildignore+=*/Assets/*.meta

" Disable swap file. Some people say to keep swap file enabled but in a
" temporary folder instead. I dislike the dialog that pops up every now and
" then if a swapfile is left so I just leave it fully disabled
set noswapfile

" Enable line numbers
set number

" Don't wrap lines
set nowrap

" Let backspace delete indentations, newlines, and don't make it stop after
" reaching the start of your insert mode
set backspace=indent,eol,start

" Incremental search. start searching and moving through the file while typing
" the search phrase
set incsearch

" Other options that I just copied and haven't tried understanding yet
set showmode
set nocompatible
set wildmenu
set lazyredraw
set hidden
set softtabstop=4
set tabstop=4
set shiftwidth=4
set expandtab
set autoindent
set smartindent
set cindent
set mouse=a
set cursorline
set numberwidth=6
set encoding=utf-8

" Enable relative line numbering
set rnu

" Disable mode line
set nomodeline

" Customize our status line
set statusline=%f%m%r%h%w\
set statusline+=[%{&ff}]
set statusline+=\ %{coc#status()}
set statusline+=%=
set statusline+=[\%03.3b/\%02.2B]\ [POS=%04v]

set laststatus=2
let g:falcon_lightline = 1
"let g:lightline.colorscheme = 'falcon'

if &t_Co == 256
    " If we're on a 256-color terminal
    "colorscheme pixelmuerto
    set termguicolors
    "let g:alduin_Shout_Dragon_Aspect = 1
    set background=dark
    let base16colorspace=256
    colorscheme falcon
else
    " Else fall back to ron
    colorscheme ron
    hi CursorLine term=bold cterm=bold guibg=Grey40
endif

" Make a slight customization with the cursorline to the ron theme
set cursorline

" Store an undo buffer in a file in nvims default folder ($XDG_DATA_HOME/nvim/undo)
set undofile
set undolevels=1000
set undoreload=10000

" Use ; as :
" Very convenient as you don't have to press shift to run commands
nnoremap ; :

" Unbind Q (it used to take you into Ex mode)
nnoremap Q <nop>

" Unbind F1 (it used to show you a help menu)
nnoremap <F1> <nop>

" Unbind Shift+K, it's previously used for opening manual or help or something
map <S-k> <Nop>

nnoremap <silent> <F5> :lnext<CR>
nnoremap <silent> <F6> :lprev<CR>
nnoremap <silent> <C-Space> :ll<CR>

let g:ale_linters = {
            \ 'cpp': ['clangcheck', 'clangtidy', 'clang-format', 'clazy', 'cquery', 'uncrustify'],
            \ 'go': ['staticcheck'],
            \ 'typescript': ['tsserver'],
            \ 'typescriptreact': ['tsserver'],
            \ 'javascript': ['eslint'],
            \ 'javascriptreact': ['eslint'],
            \ 'python': ['flake8', 'mypy'],
            \ }
let g:ale_cpp_clangtidy_extra_options = '-std=c++20 -lstdc++fs'
let g:ale_cpp_cc_options = '-std=c++20 -Wall -lstdc++fs'
let g:ale_cpp_gcc_options = '-std=c++20 -Wall'
let g:ale_cpp_clang_options = '-std=c++20 -Wall'
let g:ale_set_quickfix = 0

let g:ale_go_staticcheck_lint_package = 1

let mapleader = "\<Space>"
let g:go_fmt_command = "gofmt"
let g:go_fmt_options = {
            \ 'gofmt': '-s',
            \ }

au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <leader>c <Plug>(go-coverage)
au FileType go nmap <leader>d <Plug>(go-doc)
au FileType go nmap <leader>e :GoIfErr<CR>

au FileType cpp nmap <leader>c :call SyntasticCheck()<CR>
au FileType cpp nmap <leader>f <Plug>(operator-clang-format)
au FileType cpp nmap <leader>h :call CurtineIncSw()<CR>
au FileType c nmap <leader>f <Plug>(operator-clang-format)
au FileType c nmap <leader>h :call CurtineIncSw()<CR>

" Python leader-bindings (Space+Key)
au FileType python nmap <leader>f <Plug>(ale_fix)

au FileType javascript setlocal ts=2 sw=2 sts=2
au FileType html setlocal ts=2 sw=2 sts=2

autocmd! BufWritePost *.go :GoImports

let g:ctrlp_working_path_mode = 'rwa'
let g:ctrlp_show_hidden = 1 " show dot files

nnoremap <silent> <leader>b :CtrlPBuffer<CR>
nnoremap <silent> <C-K> :CtrlPMixed<CR>
nnoremap <silent> <C-Y> :CtrlPTag<CR>

" clang-format extension options
autocmd FileType c ClangFormatAutoEnable
autocmd FileType cpp ClangFormatAutoEnable
autocmd FileType javascript ClangFormatAutoDisable

" clang_complete
let g:clang_library_path='/usr/local/lib/libclang.so'
let g:clang_auto_select=1
let g:clang_close_preview=1

let g:ale_fixers = {
            \ 'python': ['black'],
            \ 'css': ['prettier'],
            \ 'scss': ['prettier'],
            \ 'json': ['prettier'],
            \ 'html': [],
            \ 'typescript': ['eslint', 'prettier'],
            \ 'typescriptreact': ['eslint', 'prettier'],
            \ 'javascript': ['eslint', 'prettier'],
            \ 'javascriptreact': ['eslint', 'prettier'],
            \ 'rust': ['rustfmt'],
            \ 'markdown': ['prettier'],
            \ }

" YouCompleteMe
let g:ycm_python_binary_path = '/usr/bin/python3'
let g:ycm_key_list_stop_completion = ['<C-y>', '<CR>']

let g:rustfmt_autosave = 1

au FocusGained,BufEnter * :silent! checktime

let g:clang_format#enable_fallback_style = 1

let g:vim_isort_python_version = 'python3'

" SPACE+Y = Yank  (SPACE being leader)
" SPACE+P = Paste
vmap <silent> <leader>y "+y
vmap <silent> <leader>p "+p
map <silent> <leader>p "+p

" Graphviz
"" Compile .dot-files to png
let g:graphviz_output_format = 'png'

"" Open Graphviz results with sxiv
let g:graphviz_viewer = 'sxiv'

"" Automatically compile dot files when saving
"" XXX: For some reason, setting the output format is not respected so I need to specify png here too
autocmd BufWritePost *.dot GraphvizCompile png


if executable('cquery')
    au User lsp_setup call lsp#register_server({
                \ 'name': 'cquery',
                \ 'cmd': {server_info->['cquery']},
                \ 'root_uri': {server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), 'compile_commands.json'))},
                \ 'initialization_options': { 'cacheDirectory': $HOME.'/.cache/cquery' },
                \ 'whitelist': ['c', 'cpp', 'objc', 'objcpp', 'cc'],
                \ })
endif

nn <f2> :LspRename<cr>
nn <silent> <C-h> :LspHover<cr>
nn <silent> <C-g> :LspPeekDefinition<cr>

" Macro @e will take your default register (whatever you last yanked) and create an ostream operator for it as if it's an enum.
" This expected your register to be formatted like this:
" A,
" B,
" C,
" Enums with values do not work.
let @e='POinline std::ostream &operator<<(std::ostream &os, Type v){ýadd}i}%oswitch (v) {ýa%O}ýa%jVi{:norm ^veyIcase Type::$i: return os << "p$s";joreturn os << static_cast<int>(v);'

let g:lsp_preview_keep_focus = 0

let g:ale_fix_on_save = 1

set clipboard=

let g:coc_enable_locationlist = 0

let g:ale_list_window_size = 2

let g:ale_set_quickfix = 1

let g:ale_rust_rustfmt_options = "--edition 2018"

set list
set listchars=tab:\ \ ,trail:·,extends:>

if $LEVELS_OF_VIM
    let $LEVELS_OF_VIM = $LEVELS_OF_VIM+1
else
    let $LEVELS_OF_VIM = 1
endif

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
    elseif (coc#rpc#ready())
        call CocActionAsync('doHover')
    else
        execute '!' . &keywordprg . " " . expand('<cword>')
    endif
endfunction

nmap <silent> <leader>aj :ALENext<cr>
nmap <silent> <leader>ak :ALEPrevious<cr>

nnoremap <nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
nnoremap <nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
inoremap <nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
inoremap <nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"

nmap <silent> <tab> <C-^>

" Use ctrl-[hjkl] to select the active split!
nmap <silent> <c-k> :wincmd k<CR>
nmap <silent> <c-j> :wincmd j<CR>
nmap <silent> <c-h> :wincmd h<CR>
nmap <silent> <c-l> :wincmd l<CR>

set ignorecase

" Use Ctrl-S for saving
noremap <silent> <C-S>          :update<CR>
vnoremap <silent> <C-S>         <C-C>:update<CR>
inoremap <silent> <C-S>         <C-O>:update<CR>

" Run mkdir -p if directory for the current file doesn't exist
augroup Mkdir
    autocmd!
    autocmd BufWritePre * call mkdir(expand("<afile>:p:h"), "p")
augroup END

" Automatically remove trailing whitespace
autocmd FileType c,cpp,java,php,javascript,typescript,javascriptreact,python autocmd BufWritePre <buffer> %s/\s\+$//e

" Linux system clipboard sharing
set clipboard=unnamed

" Multi-cursor settings
let g:VM_mouse_mappings    = 1
let g:VM_theme             = 'iceblue'

let g:VM_maps = {}
let g:VM_maps["Undo"]      = 'u'
let g:VM_maps["Redo"]      = '<C-r>'

" Alignment
command! -nargs=? -range Align <line1>,<line2>call AlignSection('<args>')
vnoremap <silent> <Leader>a :Align<CR>
function! AlignSection(regex) range
  let extra = 1
  let sep = empty(a:regex) ? '=' : a:regex
  let maxpos = 0
  let section = getline(a:firstline, a:lastline)
  for line in section
    let pos = match(line, ' *'.sep)
    if maxpos < pos
      let maxpos = pos
    endif
  endfor
  call map(section, 'AlignLine(v:val, sep, maxpos, extra)')
  call setline(a:firstline, section)
endfunction
function! AlignLine(line, sep, maxpos, extra)
  let m = matchlist(a:line, '\(.\{-}\) \{-}\('.a:sep.'.*\)')
  if empty(m)
    return a:line
  endif
  let spaces = repeat(' ', a:maxpos - strlen(m[1]) + a:extra)
  return m[1] . spaces . m[2]
endfunction

nmap <leader>p/ :Ack 
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

" Always use Ack!
cnoreabbrev Ack Ack!

command! -nargs=0 RemoveUnusedImports call RemoveUnusedImports()
nmap <silent> <Leader>e :RemoveUnusedImports<CR>
function! RemoveUnusedImports()
    let configPath = findfile(".eslintrc-unused-imports.js", ".;")
    execute "!" . "eslint --fix -c " . configPath " " . bufname("%")
endfunction

" Reset search
nnoremap <Leader><Space> :noh<Enter>
