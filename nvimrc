call plug#begin('~/.nvim/plugged')

"colors
Plug 'tomasr/molokai'

" fzf
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'Shougo/vimproc.vim', {'do': 'make -f  make_unix.mak'}
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-repeat'
Plug 'Shougo/deoplete.nvim'
Plug 'godlygeek/tabular'
Plug 'derekwyatt/vim-scala'
Plug 'benekastah/neomake'
Plug 'derekelkins/agda-vim'
Plug 'leafgarland/typescript-vim'
Plug 'michaeljsmith/vim-indent-object'
Plug 'idris-hackers/idris-vim'
Plug 'dag/vim-fish'
Plug 'raichoo/smt-vim'
Plug 'rust-lang/rust.vim'
Plug 'raichoo/purescript-vim'

"haskell
Plug 'neovimhaskell/haskell-vim'
Plug 'pbrisbin/vim-syntax-shakespeare'
Plug 'eagletmt/neco-ghc'
Plug 'eagletmt/ghcmod-vim'
Plug 'raichoo/ghcid-neovim'

call plug#end()

filetype plugin indent on

syntax on

let mapleader="ö"
let maplocalleader="\\"

map <silent> <Leader>tr :NERDTreeToggle<cr>
map <silent> <Leader>lf :Files<cr>
map <silent> <Leader>ls :Buffers<CR>
map <silent> <Leader>lm :Marks<CR>
map <silent> <Leader>lw :Windows<CR>
map <silent> <Leader>nm :Neomake<cr>
map Y y$

function! <SID>FixWhitespaces()
  let l:search = @/
  let l:l = line('.')
  let l:c = col('.')

  %s/\s\+$//e

  let @/ = l:search
  call cursor(l:l, l:c)
endfunction

autocmd! User FzfStatusLine setlocal statusline=%#airline_z#\ FZF\ %#airline_a_to_airline_b#>%#airline_x_inactive#>
command! FixWhitespaces call <SID>FixWhitespaces()

nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'
vnoremap <expr> j v:count ? 'j' : 'gj'
vnoremap <expr> k v:count ? 'k' : 'gk'

noremap <Up> <nop>
noremap <Down> <nop>
noremap <Left> <nop>
noremap <Right> <nop>
noremap <PageUp> <nop>
noremap <PageDown> <nop>

inoremap <Up> <nop>
inoremap <Down> <nop>
inoremap <Left> <nop>
inoremap <Right> <nop>
inoremap <PageUp> <nop>
inoremap <PageDown> <nop>

nnoremap <silent> <C-l> :noh<CR><C-l>
nnoremap * *N
nnoremap # #N
nnoremap g* g*N
nnoremap g# g#N
vnoremap * *N
vnoremap # #N
vnoremap g* g*N
vnoremap g# g#N
nnoremap <silent> <Tab> :next<CR>
nnoremap <silent> <S-Tab> :previous<CR>

cnoremap <C-a> <nop>
cnoremap <C-e> <nop>
cnoremap <C-p> <nop>
cnoremap <C-n> <nop>
cnoremap <C-b> <nop>
cnoremap <M-b> <nop>
cnoremap <M-f> <nop>

set termguicolors
set cedit=<C-f>
set clipboard=unnamedplus
set mouse=nv
set hlsearch
"set ignorecase
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
set undofile
set undodir=~/.nvim/tmp/undo//
set backupdir=~/.nvim/tmp/backup//
set directory=~/.nvim/tmp/swap//
set backup
set noswapfile
set list
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮,nbsp:·
set wrap
set formatoptions+=jc
set textwidth=80

colorscheme molokai

highlight MatchParen guibg=bg guifg=#FD971F gui=Bold
highlight VertSplit guifg=#465457 guibg=#465457
highlight StatusLineNC guifg=#465457 guibg=#465457
highlight TermCursor guifg=#66D9EF
highlight TermCursorNC guifg=#FFFFFF

au BufNewFile,BufRead *.dump-stg,*.dump-simpl setf haskell
au BufNewFile,BufRead *.purs setf haskell
au BufNewFile,BufRead *.dump-cmm,*.dump-opt-cmm setf c
au BufNewFile,BufRead *.dump-asm setf asm
au BufNewFile,BufRead *.d setf dtrace
au BufNewFile,BufRead *.agda setf agda
au TermOpen term://* setlocal number | setlocal relativenumber | setlocal nolist | setlocal numberwidth=5 | setlocal nocursorline
au InsertEnter * set nocursorline
au InsertLeave * set cursorline
au vimenter * if !argc() | NERDTree | endif
au bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
au BufReadPost fugitive://* set bufhidden=delete

let NERDTreeMinimalUI = 1

let g:haskell_enable_quantification = 1
"let g:haskell_enable_recursivedo = 1
"let g:haskell_enable_arrowsyntax = 1
"let g:haskell_enable_static_pointers = 1
let g:haskell_enable_typeroles = 1
let g:haskell_enable_pattern_synonyms = 1

let g:hamlet_prevent_invalid_nesting = 0

let g:agda_extraincpaths = ["/home/raichoo/Sources/agda-stdlib/src"]

let g:deoplete#enable_at_startup = 1

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_tabs = 1
let g:airline#extensions#tabline#show_buffers = 0
let g:airline#extensions#tabline#tab_min_count = 2
let g:airline#extensions#tabline#left_sep = '>'
let g:airline#extensions#tabline#left_alt_sep = '>'
let g:airline#extensions#tabline#show_close_button = 0
let g:airline#extensions#tabline#show_tab_type = 0
