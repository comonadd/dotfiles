scriptencoding utf-8

set nocompatible
filetype off

call plug#begin(stdpath('data') . '/plugged')

" visual
Plug 'https://github.com/RRethy/vim-illuminate'

" if !has('mac')
    " Disable heavy stuff on macos
    " Linters, fixers, formatters, etc.
"     Plug 'w0rp/ale'
" endif

" Searcher
" Plug 'mileszs/ack.vim'
" Search
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'https://github.com/eugen0329/vim-esearch'

" Replace with proper case handling
Plug 'tpope/vim-abolish'

" Syntax highlight
Plug 'vim-python/python-syntax'
Plug 'justinmk/vim-syntax-extra' " C, Bison, Flex
Plug 'elzr/vim-json'
Plug 'plasticboy/vim-markdown'
Plug 'pboettch/vim-cmake-syntax'
Plug 'evanleck/vim-svelte', {'branch': 'main'}
Plug 'jparise/vim-graphql'

let g:vim_markdown_folding_disabled = 1

let g:svelte_indent_script = 0
let g:svelte_indent_style = 0
let g:svelte_preprocessors = ['typescript']

" Change surrounding symbols easly using cs<FROM><TO> (e.g. cs"')
Plug 'tpope/vim-surround'

" Git stuff
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

" Python import sorter
Plug 'brentyi/isort.vim'

" Go plugin (does most things Go-related)
Plug 'fatih/vim-go'

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

" Support for different languages
Plug 'tikhomirov/vim-glsl'

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
Plug 'ayu-theme/ayu-vim'
Plug 'therubymug/vim-pyte'
Plug 'vim-scripts/eclipse.vim'
Plug 'vim-scripts/summerfruit256.vim'
Plug 'vim-scripts/AutumnLeaf'
Plug 'vim-scripts/ironman.vim'
Plug 'NLKNguyen/papercolor-theme'

" Convert between cases
Plug 'chiedo/vim-case-convert'

Plug 'preservim/nerdtree'

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

" Ignore patch files
set wildignore+=*.patch

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
let g:lightline = {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ], [ 'relativepath', 'modified' ] ],
      \ }
      \ }

if &t_Co == 256
    let base16colorspace=256
    " If we're on a 256-color terminal
    "colorscheme pixelmuerto
    set termguicolors
    "let g:alduin_Shout_Dragon_Aspect = 1
    " set background=dark
    " colorscheme falcon
    " colorscheme jellybeans
    " colorscheme molokai
    set background=dark
    colorscheme PaperColor
    " colorscheme ironman

    " let ayucolor="light"  " for light version of theme
    " let ayucolor="mirage" " for mirage version of theme
    " let ayucolor="dark"   " for dark version of theme
    " colorscheme ayu

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
            \ 'python': ['autoimport', 'black'],
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

" Coc Plugins
let g:coc_global_extensions = [
\ 'coc-css',
\ 'coc-json',
\ 'coc-python',
\ 'coc-tsserver',
\ 'coc-prettier',
\ ]

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

nnoremap <nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
nnoremap <nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
inoremap <nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
inoremap <nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"

nmap <silent> <tab> <C-^>

" Use ctrl-[hjkl] to select the active split!
" Split navigation
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

" Fugitive {
    autocmd FileType fugitive nmap <buffer> <tab> =
    autocmd FileType fugitive nmap <buffer> q gq
" }

" Linux system clipboard sharing
set clipboard=unnamed

" Multi-cursor settings
let g:VM_mouse_mappings    = 1
let g:VM_theme             = 'iceblue'

let g:VM_maps = {}
let g:VM_maps["Undo"]      = 'u'
let g:VM_maps["Redo"]      = '<C-r>'

" Fzf {
    fu! OpenCWD() abort
        let $VIM_CWD = expand('%:p:h') 
        :Files $VIM_CWD
    endfu

    let $FZF_DEFAULT_COMMAND = 'ag -g ""'
    map <leader>o :call OpenCWD()<CR>
    map <c-p> :Files<CR>
    noremap <silent><leader>l :Lines<CR>
    noremap <silent><leader>g :Ag<CR>
    nnoremap <silent> <Leader>ag :Ag <C-R><C-W><CR>
    map <leader>b :Buffers<CR>
    map <leader>h :History<CR>

    map <leader>e :Files $VIRTUAL_ENV<CR>
    map <leader>E :Ag $VIRTUAL_ENV<CR>

    map <leader>t :Tags<CR>

    map <leader>: :History:<CR>
    map <leader>/ :History/<CR>

    let g:fzf_tags_command = 'ctags -R --exclude=.git --exclude=node_modules --exclude="*static/dist*" --exclude="*static/vendor*" --exclude="*.css" --exclude="*cassettes*" --exclude="*package-lock.json*"'
    autocmd! FileType fzf tnoremap <buffer> <Esc> <c-c>
    autocmd! FileType fzf tnoremap <buffer> <leader>q <c-c>

"  esearch{

    " autocmd! FileType esearch tnoremap <buffer> <Esc> :q!<cr>
    " autocmd! FileType esearch tnoremap <buffer> q     :q!<CR>

    " \ 'prefill':        ['visual', 'hlsearch', 'last'],
    let g:esearch = {
      \ 'adapter':    'ag',
      \ 'backend':    'nvim',
      \ 'out':        'win',
      \ 'batch_size': 1000,
      \}
    let g:esearch.win_map = [
      \ ['n', 'x',       ':split'],
      \ ['n', 'yf',      ':call setreg(esearch#util#clipboard_reg(), b:esearch.filename())<cr>'],
      \ ['n', 't',       ':call b:esearch.open("NewTabdrop")<cr>'                              ],
      \ ['n', '+',       ':call esearch#init(extend(b:esearch, AddAfter(+v:count1)))<cr>'      ],
      \ ['n', '-',       ':call esearch#init(extend(b:esearch, AddAfter(-v:count1)))<cr>'      ],
      \ ['n', 'gq',      ':call esearch#init(extend(copy(b:esearch), {"out": "qflist"}))<cr>'  ],
      \ ['n', 'gsp',     ':call esearch#init(extend(b:esearch, sort_by_path))<cr>'             ],
      \ ['n', 'gsd',     ':call esearch#init(extend(b:esearch, sort_by_date))<cr>'             ],
      \ ['n', 'q',       ':q!<cr>'],
    \]
      "\ ['n', '<Esc>',   ':q!<cr>'],
    " let g:esearch#adapter#ag#options = '--ignore="*dist*" --ignore=".tags*"'

    fu! EsearchInFiles(argv) abort
      " let original = g:esearch#adapter#ag#options
      call esearch#init(a:argv)
      " let g:esearch#adapter#ag#options = original
    endfu

    noremap  <silent><leader>ff :<C-u>call EsearchInFiles({})<CR>
    xnoremap <silent><leader>ff :<C-u>call EsearchInFiles({'visualmode': 1})<CR>

" }

nnoremap <C-w> :q!<CR>

" Cocnvim {

    " if hidden is not set, TextEdit might fail.
    set hidden

    " Some servers have issues with backup files, see #649
    set nobackup
    set nowritebackup

    " Better display for messages
    set cmdheight=2

    " You will have bad experience for diagnostic messages when it's default 4000.
    set updatetime=300

    " don't give |ins-completion-menu| messages.
    set shortmess+=c

    " always show signcolumns
    set signcolumn=yes

    " Use tab for trigger completion with characters ahead and navigate.
    " Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
    inoremap <silent><expr> <TAB>
          \ pumvisible() ? "\<C-n>" :
          \ <SID>check_back_space() ? "\<TAB>" :
          \ coc#refresh()
    inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

    function! s:check_back_space() abort
      let col = col('.') - 1
      return !col || getline('.')[col - 1]  =~# '\s'
    endfunction

    " Use <c-space> to trigger completion.
    inoremap <silent><expr> <c-space> coc#refresh()

    " Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
    " Coc only does snippet and additional edit on confirm.
    inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
    " Or use `complete_info` if your vim support it, like:
    " inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

    " Use `[g` and `]g` to navigate diagnostics
    nmap <silent> [g <Plug>(coc-diagnostic-prev)
    nmap <silent> ]g <Plug>(coc-diagnostic-next)

    " Remap keys for gotos
    nmap <silent> gd <Plug>(coc-definition)
    nmap <silent> gy <Plug>(coc-type-definition)
    nmap <silent> gi <Plug>(coc-implementation)
    nmap <silent> gr <Plug>(coc-references)

    " Search workspace symbols.
    nnoremap <silent><nowait> <Leader>s  :<C-u>CocList -I symbols<cr>

    " Use K to show documentation in preview window
    nnoremap <silent> K :call <SID>show_documentation()<CR>

    " CocFix
    nmap <silent> gk :CocFix<CR>

    function! s:show_documentation()
      if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
      else
        call CocAction('doHover')
      endif
    endfunction

    " This makes it so that you can click a variable and a float window pops up
    command! -nargs=0 ShowInfoOnCursor call CocActionAsync('doHover')
    nmap <silent> gl :ShowInfoOnCursor<CR>

" }

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
nnoremap <Leader>uu :RemoveUnusedImports<Enter>

" Reset search
nnoremap <Leader><Space> :noh<Enter>

fun! Confirm(msg)
    echo a:msg . ' '
    let l:answer = nr2char(getchar())

    if l:answer ==? 'y'
        return 1
    elseif l:answer ==? 'n'
        return 0
    else
        echo 'Please enter "y" or "n"'
        return Confirm(a:msg)
    endif
endfun

fun! ConfirmDeleteCurrentFile()
    if Confirm("Do you really want to completely delete current file?")
        call delete(expand("%")) | bdelete!
    endif
endfun

" Delete current file completely
nnoremap <Leader>fd :call ConfirmDeleteCurrentFile()<CR>

" GLSL file formats
autocmd! BufNewFile,BufRead *.vs,*.fs,*.glsl set ft=glsl

fun! OpenInOtherSplit()
    wincmd W
    let filename = expand('%F')
    execute "edit " . filename
endfun

nnoremap <silent> <c-q> :call OpenInOtherSplit()<CR>

" NERDTree {
    let NERDTreeIgnore = ['\.pyc$']
    let NERDTreeShowHidden=1
    " enable line numbers
    let NERDTreeShowLineNumbers=1
    " make sure relative line numbers are used
    autocmd FileType nerdtree setlocal relativenumber
    let g:NERDTreeWinSize=35
    let NERDTreeQuitOnOpen=1

    " mapping
    map <Leader>m :NERDTreeFind<CR>z.
    map <C-o> :NERDTreeToggle %<CR>

    let NERDTreeMapOpenVSplit='v'
    let NERDTreeMapOpenSplit='x'
    let NERDTreeMapCloseDir='i'
    let NERDTreeMapPreview="f"
    let NERDTreeMapHelp='<f12>'
" }

command! -nargs=0 Prettier :CocCommand prettier.formatFile

set clipboard=unnamedplus
