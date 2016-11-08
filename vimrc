" start plugins
execute pathogen#infect()

" space is Leader
map <space> <leader>

""" Visual

colorscheme torte " set black color scheme

syntax enable " enable syntax processing

set nocursorline " don't highlight current line

set nohlsearch " disable search highlight

set showmatch " highlight matching brackets

set lazyredraw " redraw only when necessary (faster macros)

""" Tabs

set tabstop=4 " number of visual spaces per TAB

set softtabstop=4 " number of spaces in tab when editing

set shiftwidth=4 " number of space when using > or <

set expandtab " tabs are spaces


""" Numbering

set number " show line numbers

set rnu " show relative line numbers

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

nnoremap <Leader>n :call NumberToggle()<CR>


""" Command menu and Searching

set wildmenu " visual autocomplete for command menu

set wildmode=longest,full " don't autocomplete on first tab press

set showcmd " show command in bottom bar

set incsearch " search as characters are entered

" search through subdirectories when looking for files
set path+=**


""" Folds

set foldmethod=indent " fold based on indent

set foldlevelstart=1 " foldlevel when window is loaded

set foldmethod=indent " fold based on indent level


""" Insert mode

" backspace is used to remove previous characters, indents, and newlines
set backspace=indent,eol,start

" Map Ctrl-Backspace to delete the previous word in insert mode.
imap <C-BS> <C-W>


""" Filetypes

autocmd FileType make setlocal noexpandtab " set Makefiles with tabs not spaces

filetype indent on " load filetype-specific indent files

" ensure normal tabs and 8 space tabs in assembly files
autocmd FileType asm set noexpandtab shiftwidth=8 softtabstop=0


""" System/OS

set mouse= " disable the mouse

" set shell to zsh on linux (if it exists)
if !(has("win32") || has("win16") || has("win32unix"))
    if filereadable("/bin/zsh") && $SHELL=="/bin/zsh"
        silent! set shell=/bin/zsh
    endif
endif

set updatecount=10 " swap files are rotated every 10 keystrokes

" make an undo file to allow undoing after closing a file
set undofile
set undodir=~/.vim/undodir

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

" write w/ privileges when Vim isn't started as root
cmap w!! %!sudo tee > /dev/null %

" ignore modelines
set modelines=0

" toggle background
function! ClearBG()
  highlight Normal ctermbg=none
endfunction

function! BlackBG()
  highlight Normal ctermbg=black
endfunction


""" Key shortcuts

" remove trailing whitespace and return to start position
" remove highlight if in nvim
if has('nvim')
    noremap <Leader>w :%s/\s\+$//<CR>:nohl<CR>``
else
    noremap <Leader>w :%s/\s\+$//<CR>``
endif

" <Leader>l formats a line
noremap <Leader>l Vgq

" <Leader>s toggles spelling
function! SpellToggle()
    if(&spell == 1)
        setlocal nospell
    else
        setlocal spell spelllang=en_us
    endif
endfunc

nnoremap <Leader>s :call SpellToggle()<CR>


""" Plugins

" show recently opened files
noremap <Leader>m :MRU<CR>

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

" place the vim-airline bar above the command line
set laststatus=2

" ignore trailing whitespace in markdown
autocmd FileType markdown AirlineToggleWhitespace


""" Neovim

if has('nvim')
    " Esc returns to normal mode in terminal mode
    tnoremap <C-w> <C-\><C-n><C-w>
endif
