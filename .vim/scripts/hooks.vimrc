" Make so that the trailing whitespaces get removed after the buffer save
autocmd BufWritePre *.* :%s/\s\+$//e

" Make *.tsx files to be detected as TSX files
autocmd BufNewFile,BufRead *.tsx set filetype=typescript.jsx

" Make Vim configuration to auto-reload after the write
augroup myvimrc
  au!
  au BufWritePost .vimrc,_vimrc,vimrc,.gvimrc,_gvimrc,gvimrc so $MYVIMRC | if has('gui_running') | so $MYGVIMRC | endif
augroup END

" Enable directory auto-change
autocmd BufEnter * silent! lcd %:p:h

" Settings for C mode
augroup project
  autocmd!
  autocmd BufRead,BufNewFile *.h,*.c set filetype=c.doxygen
augroup END

" Settings for C++ mode
augroup project
  autocmd!
  autocmd BufRead,BufNewFile *.hpp,*.cpp set filetype=cpp.doxygen
augroup END

if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1
