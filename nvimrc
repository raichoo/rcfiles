call plug#begin('~/.nvim/plugged')

Plug 'kien/ctrlp.vim'
Plug 'bling/vim-airline'
Plug 'scrooloose/nerdtree'
"Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/syntastic'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'tomasr/molokai'
Plug 'Shougo/neocomplcache.vim'
Plug 'godlygeek/tabular'
Plug 'raichoo/snipmate.vim'

"haskell
Plug 'raichoo/haskell-vim'
Plug 'pbrisbin/vim-syntax-shakespeare'

Plug 'idris-hackers/idris-vim'

call plug#end()

filetype plugin indent on

syntax on

let mapleader="ö"
let maplocalleader="ä"

map <silent> <Leader>tr :NERDTreeToggle<cr>
map <silent> <Leader>tf :NERDTreeFocus<cr>
map <silent> <Leader>ls :CtrlPBuffer<cr>

command! FixWhitespaces %s/\s\+$//g

nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

noremap <Up> <nop>
noremap <Down> <nop>
noremap <Left> <nop>
noremap <Right> <nop>
nnoremap <silent> <C-l> :noh<CR><C-l>
nnoremap * *N
nnoremap # #N
vnoremap * *N
vnoremap # #N
nnoremap <silent> <Tab> :bnext<CR>
nnoremap <silent> <S-Tab> :bprevious<CR>
nnoremap / /\v
vnoremap / /\v
nnoremap ? ?\v
vnoremap ? ?\v

cnoremap <C-a> <Home>
cnoremap <C-e> <end>
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
cnoremap <M-b> <S-Left>
cnoremap <M-f> <S-Right>

set clipboard=unnamedplus
set mouse=nv
set hlsearch
set ignorecase
set hidden
set background=dark
set incsearch
set linebreak
set ruler
set showcmd
set noshowmode
set shiftwidth=2
set tabstop=2
set backspace=2
set relativenumber
set number
set encoding=utf-8
set expandtab
set smarttab
set cmdheight=1
set laststatus=2
set cursorline
set undodir=~/.nvim/tmp/undo//
set backupdir=~/.nvim/tmp/backup//
set directory=~/.nvim/tmp/swap//
set backup
set noswapfile
set list
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
set nowrap
set foldenable
set foldnestmax=10

colorscheme molokai
highlight MatchParen guibg=bg guifg=#FD971F gui=Bold
highlight VertSplit guifg=#465457 guibg=#465457
highlight StatusLineNC guifg=#465457 guibg=#465457

function! CSettings()
  set noexpandtab
  set shiftwidth=4
  set tabstop=4
endfunction

au BufNewFile,BufRead *.d setf dtrace
au BufNewFile,BufRead *.agda setf agda
au BufNewFile,BufRead *.c,*.cpp call CSettings()
au InsertEnter * set nocursorline
au InsertLeave * set cursorline

au vimenter * if !argc() | NERDTree | endif
au bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

au BufReadPost fugitive://* set bufhidden=delete

let NERDTreeMinimalUI = 1

let g:syntastic_check_on_wq = 0
let g:syntastic_mode_map = { 'mode': 'passive',
                           \ 'active_filetypes': [],
                           \ 'passive_filetypes': [] }

let g:NERDCustomDelimiters = {
    \ 'idris': { 'left': '{-', 'right': '-}', 'leftAlt': '--' }
\}

let g:haskell_enable_quantification = 1
"let g:haskell_enable_recursivedo = 1
"let g:haskell_enable_arrowsyntax = 1
"let g:haskell_enable_static_pointers = 1
let g:haskell_enable_typeroles = 1
let g:haskell_enable_pattern_synonyms = 1

let g:hamlet_prevent_invalid_nesting = 0

let g:ctrlp_map = '<Leader> :CtrlP<cr>'

let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_smart_case = 1

if !exists('g:neocomplcache_keyword_patterns')
  let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'

inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_tabs = 1
let g:airline#extensions#tabline#show_buffers = 0
let g:airline#extensions#tabline#tab_min_count = 2
let g:airline#extensions#tabline#left_sep = '>'
let g:airline#extensions#tabline#left_alt_sep = '>'
let g:airline#extensions#tabline#show_close_button = 0
let g:airline#extensions#tabline#show_tab_type = 0
