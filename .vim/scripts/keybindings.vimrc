" Window-management-related keybindings
nnoremap <leader>wv <C-W>v
nnoremap <leader>wd <C-w>c
nnoremap <leader>1 <C-w>1w
nnoremap <leader>2 <C-w>2w
nnoremap <leader>3 <C-w>3w
nnoremap <leader>4 <C-w>4w
nnoremap <leader>5 <C-w>5w
nnoremap <leader>6 <C-w>6w
nnoremap <leader>7 <C-w>7w
nnoremap <leader>8 <C-w>8w
nnoremap <leader>9 <C-w>9w
nnoremap <leader>w+ <C-w>+
nnoremap <leader>w- <C-w>-
nnoremap <leader>w> <C-w>>
nnoremap <leader>w< <C-w><
nnoremap <leader>w_ <C-w>_
nnoremap <leader>w\| <C-w>\|
nnoremap <leader>w= <C-w>=

" Buffer-management-related keybindings
nnoremap <leader><Tab> :call SwitchBuffer()<CR>

" Editing keybindings
vnoremap <C-c> "*y

inoremap <C-v> <Esc>"*pa
nnoremap <C-v> "*p

inoremap <C-x> <Esc>"*dda
nnoremap <C-x> "*dd
vnoremap <C-x> "*d

nnoremap <leader>dl 0d$
nnoremap <leader>g i<cr><Esc>

" Calculation keybindings
ino <C-A> <C-O>yiW<End>=<C-R>=<C-R>0<CR>

" NERDTree
nnoremap <leader>b :NERDTreeToggle<CR>

" Other
nnoremap <leader>e :e <C-R>=expand('%:p:h') . '/'<CR>
