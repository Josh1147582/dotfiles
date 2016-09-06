" start plugins
execute pathogen#infect()

" set black color scheme
colorscheme torte
" enable syntax processing
syntax enable

" number of visual spaces per TAB
set tabstop=4

" number of spaces in tab when editing
set softtabstop=4

" number of space when using > or <
set shiftwidth=4

" tabs are spaces
set expandtab

" show line numbers
set number

" show relative line numbers
set rnu

" show command in bottom bar
set showcmd

" don't highlight current line
set nocursorline

" load filetype-specific indent files
filetype indent on

" visual autocomplete for command menu
set wildmenu

" don't autocomplete on first tab press
set wildmode=list,full

" redraw only when necessary (faster macros)
set lazyredraw

" highlight matching brackets
set showmatch

" search as characters are entered
set incsearch

" fold based on indent
set foldmethod=indent

" foldlevel when window is loaded
set foldlevelstart=1

" fold based on indent level
set foldmethod=indent

" disable the mouse
set mouse=

" toggle relative line nums when focus is gained/lost
:au FocusLost * :set norelativenumber
:au FocusGained * :set relativenumber

" map C-n to toggle line nums
function! NumberToggle()
    if(&relativenumber == 1)
        set norelativenumber
    else
        set relativenumber
    endif
endfunc

nnoremap <C-n> :call NumberToggle()<CR>

" set shell to zsh on linux (if it exists)
if !(has("win32") || has("win16") || has("win32unix"))
    if filereadable("/bin/zsh") && $SHELL=="/bin/zsh"
        silent! set shell=/bin/zsh
    endif
endif

" swap files are rotated every 10 keystrokes
set updatecount=10

" backspace is used to remove previous characters, indents, and newlines
set backspace=indent,eol,start

" Map Ctrl-Backspace to delete the previous word in insert mode.
imap <C-BS> <C-W>

" make an undo file to allow undoing after closing a file
set undofile
set undodir=~/.vim/undodir

" set Makefiles with tabs not spaces
autocmd FileType make setlocal noexpandtab

" write w/ privileges when Vim isn't started as root
cmap w!! %!sudo tee > /dev/null %

" toggle background
function! ClearBG()
  highlight Normal ctermbg=none
endfunction

function! BlackBG()
  highlight Normal ctermbg=black
endfunction

" leader

" space is Leader
map <space> <leader>

" compare current buffer to saved file
function! s:DiffWithSaved()
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
com! DiffSaved call s:DiffWithSaved()

" map the comp buff function above
noremap <Leader>d :DiffSaved<CR>

" <Leader>l formats a line
noremap <Leader>l Vgq

" remove trailing whitespace and return to start position
" remove lighlight if in nvim
if has('nvim')
    noremap <Leader>w :%s/\s\+$//<CR>:nohl<CR>``
else
    noremap <Leader>w :%s/\s\+$//<CR>``
endif

" plugins

" show recently opened files
noremap <Leader>m :MRU<CR>

" Start Geeknote
noremap <Leader>g :Geeknote<CR>

" show undo tree
noremap <Leader>u :UndotreeToggle<CR>

" syntastic/YCM
if exists(':SyntasticStatuslineFlag()')
    set statusline+=%#warningmsg#
    set statusline+=%{SyntasticStatuslineFlag()}
    set statusline+=%*

    let g:syntastic_always_populate_loc_list = 1
    let g:syntastic_auto_loc_list = 1
    let g:syntastic_check_on_open = 1
    let g:syntastic_check_on_wq = 0
endif

" YouCompleteMe
let g:ycm_global_ycm_extra_conf = '/home/josh/.vim/bundle/ycm_extra_conf.py'

" autoclose suggestion windows
let g:ycm_autoclose_preview_window_after_insertion=1

" colors
highlight YcmWarningSection ctermfg=Yellow
highlight YcmWarningSign ctermfg=Yellow

highlight YcmErrorSection ctermfg=Red
highlight YcmErrorsign ctermfg=Red

" vim-airline
" place the airline bar above the command line
set laststatus=2

" neovim

if has('nvim')
    " Esc clears search highlight
    nnoremap <silent> <esc> :noh<cr><esc>

    " Esc returns to normal mode in terminal mode
    tnoremap <C-w> <C-\><C-n><C-w>
endif
