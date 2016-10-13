"Setting colorscheme BEG"
color desert
"Setting colorscheme END"

"Setting flag variables BEG"
set smarttab
set expandtab
set number
set cursorline
set cursorline
set list
"Setting flag variables END"

"Setting value variables BEG"
set tabstop=4
set softtabstop=4
set shiftwidth=4
set listchars=tab:>-,trail:~
set backspace=indent,eol,start
"Setting value variables END"

"Keys mapping BEG"
imap <C-BS> <C-W>
"Keys mapping END"

"Main stuff BEG"
hi cursorline term=bold cterm=bold guibg=black
"Main stuff END"
