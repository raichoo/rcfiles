let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 0

function! EnvSetup() abort
  if filereadable('stack.yaml')
    let $STACK_PROJECT_ROOT = $PWD
    let $GHC_PACKAGE_PATH = systemlist('stack exec printenv GHC_PACKAGE_PATH')[0]
    let $PATH = systemlist('stack exec printenv PATH')[0]
  endif
endfunction

call EnvSetup()

call plug#begin('~/.nvim/plugged')

"colors
Plug 'raichoo/monodark'

" fzf
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
" Plug 'w0rp/ale'
Plug 'vim-airline/vim-airline'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-repeat'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'godlygeek/tabular'
Plug 'benekastah/neomake'
Plug 'michaeljsmith/vim-indent-object'
Plug 'dag/vim-fish'
Plug 'raichoo/smt-vim'
Plug 'raichoo/purescript-vim'
" Plug 'mbbill/undotree'
Plug 'takac/vim-hardtime'

"haskell
Plug 'neovimhaskell/haskell-vim'
Plug 'pbrisbin/vim-syntax-shakespeare'
Plug 'eagletmt/neco-ghc'


"rust
Plug 'rust-lang/rust.vim'
" Plug 'racer-rust/vim-racer'

call plug#end()

filetype plugin indent on

let mapleader="ö"
let maplocalleader="\\"

map <silent> <Leader>tr :NERDTreeToggle<cr>
map <silent> <Leader>tf :NERDTreeFind<cr>
map <silent> <Leader>lf :Files<cr>
map <silent> <Leader>lg :GFiles<cr>
map <silent> <Leader>ls :Buffers<CR>
map <silent> <Leader>lm :Marks<CR>
map <silent> <Leader>lw :Windows<CR>
map <silent> <Leader>nm :Neomake<cr>

map Y y$
map <silent> <C-w>z :execute 'resize ' . line('$')<CR>

function! FixWhitespaces() abort
  let l:search = @/
  let l:l = line('.')
  let l:c = col('.')

  %s/\s\+$//e

  let @/ = l:search
  call cursor(l:l, l:c)
endfunction

autocmd! User FzfStatusLine setlocal statusline=%#airline_z#\ FZF\ %#airline_a_to_airline_b#>%#airline_x_inactive#>
command! FixWhitespaces call FixWhitespaces()

tnoremap <C-l> <C-\><C-n>
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
inoremap <C-l> <Esc>

function! HighlightSearch(word) abort
  let l:w = expand('<cword>')

  if a:word
    let @/ = '\<' . l:w . '\>'
  else
    let @/ = l:w
  endif
endfunction

nnoremap <silent> <C-l> :noh<CR><C-l>
nnoremap <silent> * :call HighlightSearch(1)<CR>:let v:searchforward=1<CR>:set hlsearch<CR>
nnoremap <silent> # :call HighlightSearch(1)<CR>:let v:searchforward=0<CR>:set hlsearch<CR>
nnoremap <silent> g* :call HighlightSearch(0)<CR>:let v:searchforward=1<CR>:set hlsearch<CR>
nnoremap <silent> g# :call HighlightSearch(0)<CR>:let v:searchforward=0<CR>:set hlsearch<CR>
vnoremap <silent> * :call HighlightSearch(1)<CR>:let v:searchforward=1<CR>:set hlsearch<CR>
vnoremap <silent> # :call HighlightSearch(1)<CR>:let v:searchforward=0<CR>:set hlsearch<CR>
vnoremap <silent> g* :call HighlightSearch(0)<CR>:let v:searchforward=1<CR>:set hlsearch<CR>
vnoremap <silent> g# :call HighlightSearch(0)<CR>:let v:searchforward=1<CR>:set hlsearch<CR>
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
set nojoinspaces
set splitright
set inccommand=nosplit
set noerrorbells
set novisualbell
set visualbell t_bv=
set cedit=<C-f>
set clipboard=unnamed,unnamedplus
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
set relativenumber number
set expandtab
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
set textwidth=80
set shortmess+=I

colorscheme monodark

if !exists('g:haskell_rebuild_tags')
  let g:haskell_rebuild_tags = 0
endif

function! HaskellRebuildTagsFinished(job_id, data, event) abort
  let g:haskell_rebuild_tags = 0
endfunction

let g:HaskellTagsHandler = {
      \ 'on_exit': function('HaskellRebuildTagsFinished')
      \ }

function! HaskellRebuildTags() abort
  if g:haskell_rebuild_tags == 0 && filereadable('stack.yaml')
    let l:cmd = 'hasktags --ignore-close-implementation --ctags .; sort tags'
    let g:haskell_rebuild_tags = jobstart(l:cmd, g:HaskellTagsHandler)
  endif
endfunction

au BufNewFile,BufRead *.dump-stg,*.dump-simpl setf haskell
au BufNewFile,BufRead *.dump-cmm,*.dump-opt-cmm setf c
au BufNewFile,BufRead *.dump-asm setf asm
au BufWritePost *.hs call HaskellRebuildTags()
au TermOpen term://* setlocal nolist | setlocal numberwidth=5 | setlocal nocursorline
au InsertEnter,WinEnter * set nocursorline
au InsertLeave,WinEnter * set cursorline
au BufReadPost fugitive://* set bufhidden=delete

let NERDTreeMinimalUI = 1

let g:haskell_enable_quantification = 1
"let g:haskell_enable_recursivedo = 1
"let g:haskell_enable_arrowsyntax = 1
"let g:haskell_enable_static_pointers = 1
let g:haskell_enable_typeroles = 1
let g:haskell_enable_pattern_synonyms = 1
" let g:haskell_classic_highlighting = 1
let g:haskell_indent_case_alternative = 1

let g:hamlet_prevent_invalid_nesting = 0

let g:deoplete#enable_at_startup = 1

let g:monodark_disable_background = 1

" let g:airline_powerline_fonts = 1
let g:airline_extensions = ['whitespace', 'tabline']
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_tabs = 1
let g:airline#extensions#tabline#show_buffers = 0
let g:airline#extensions#tabline#tab_min_count = 2
let g:airline#extensions#tabline#left_sep = '>'
let g:airline#extensions#tabline#left_alt_sep = '>'
let g:airline#extensions#tabline#show_close_button = 0
let g:airline#extensions#tabline#show_tab_type = 0
let g:airline#extensions#tabline#buffer_idx_mode = 1

nmap <leader>1 <Plug>AirlineSelectTab1
nmap <leader>2 <Plug>AirlineSelectTab2
nmap <leader>3 <Plug>AirlineSelectTab3
nmap <leader>4 <Plug>AirlineSelectTab4
nmap <leader>5 <Plug>AirlineSelectTab5
nmap <leader>6 <Plug>AirlineSelectTab6
nmap <leader>7 <Plug>AirlineSelectTab7
nmap <leader>8 <Plug>AirlineSelectTab8
nmap <leader>9 <Plug>AirlineSelectTab9

let g:hardtime_default_on = 1
