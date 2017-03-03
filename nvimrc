let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 0

if filereadable('stack.yaml')
  let $STACK_PROJECT_ROOT = $PWD
  let $GHC_PACKAGE_PATH = systemlist('stack exec printenv GHC_PACKAGE_PATH')[0]
  let $PATH = systemlist('stack exec printenv PATH')[0]
endif

call plug#begin('~/.nvim/plugged')

Plug 'raichoo/monodark'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'vim-airline/vim-airline'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-repeat'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
Plug 'neomake/neomake'
Plug 'dag/vim-fish'
Plug 'raichoo/smt-vim'
Plug 'raichoo/purescript-vim'
Plug 'takac/vim-hardtime'
Plug 'michaeljsmith/vim-indent-object'
Plug 'neovimhaskell/haskell-vim'
Plug 'pbrisbin/vim-syntax-shakespeare'
Plug 'eagletmt/neco-ghc'
Plug 'rust-lang/rust.vim'

call plug#end()

filetype plugin indent on

let mapleader="ö"
let maplocalleader="\\"

if isdirectory('.git')
  set grepprg=git\ grep\ --no-color\ --line-number
  map <silent> <Leader>lf :GFiles --others --cached --exclude-standard<cr>
else
map <silent> <Leader>lf :Files<cr>
endif

map <silent> <Leader>tr :NERDTreeToggle<cr>
map <silent> <Leader>ls :Buffers<CR>
map <silent> <Leader>lm :Marks<CR>
map <silent> <Leader>lw :Windows<CR>

map <silent> [a :prev<CR>
map <silent> ]a :next<CR>
map <silent> [A :first<CR>
map <silent> ]A :last<CR>

map <silent> [q :cprev<CR>
map <silent> ]q :cnext<CR>
map <silent> [Q :cfirst<CR>
map <silent> ]Q :clast<CR>

map <silent> [l :lprev<CR>
map <silent> ]l :lnext<CR>
map <silent> [L :lfirst<CR>
map <silent> ]L :llast<CR>

map Y y$
map <silent> & :&&<CR>

map <silent> <C-w>z :execute 'resize ' . line('$')<CR>

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

nnoremap <silent> * :call HighlightSearch(1)<CR>:let v:searchforward=1<CR>:set hlsearch<CR>
nnoremap <silent> # :call HighlightSearch(1)<CR>:let v:searchforward=0<CR>:set hlsearch<CR>
nnoremap <silent> g* :call HighlightSearch(0)<CR>:let v:searchforward=1<CR>:set hlsearch<CR>
nnoremap <silent> g# :call HighlightSearch(0)<CR>:let v:searchforward=0<CR>:set hlsearch<CR>
vnoremap <silent> * :call HighlightSearch(1)<CR>:let v:searchforward=1<CR>:set hlsearch<CR>
vnoremap <silent> # :call HighlightSearch(1)<CR>:let v:searchforward=0<CR>:set hlsearch<CR>
vnoremap <silent> g* :call HighlightSearch(0)<CR>:let v:searchforward=1<CR>:set hlsearch<CR>
vnoremap <silent> g# :call HighlightSearch(0)<CR>:let v:searchforward=1<CR>:set hlsearch<CR>

nnoremap <silent> <C-l> :noh<CR><C-l>

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
set mouse=
set hlsearch
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


function! FixWhitespaces() abort
  let l:search = @/
  let l:l = line('.')
  let l:c = col('.')

  %s/\s\+$//e

  let @/ = l:search
  call cursor(l:l, l:c)
endfunction
command! FixWhitespaces call FixWhitespaces()

function! Rename(file) abort
  let l:f = expand('%')
  execute 'saveas ' . a:file . ' | ' . 'bd! # | !rm ' . l:f
endfunction
command! -nargs=1 Rename call Rename(<f-args>)

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
au TermOpen term://* setlocal nolist " | setlocal numberwidth=5
au InsertEnter,WinEnter * set nocursorline
au InsertLeave,WinEnter * set cursorline

autocmd! User FzfStatusLine setlocal statusline=%#airline_z#\ FZF\ %#airline_a_to_airline_b#>%#airline_x_inactive#>

let NERDTreeMinimalUI = 1

let g:haskell_enable_quantification = 1
let g:haskell_enable_typeroles = 1
let g:haskell_enable_pattern_synonyms = 1
let g:haskell_indent_case_alternative = 1

let g:hamlet_prevent_invalid_nesting = 0

let g:deoplete#enable_at_startup = 1

let g:monodark_disable_background = 1

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_symbols.linenr = ''
let g:airline_symbols.maxlinenr = ''
let g:airline_symbols.whitespace = '!'
let g:airline_symbols.crypt = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = 'RO'

let g:airline_left_sep = '>'
let g:airline_left_alt_sep = '>'
let g:airline_right_sep = '<'
let g:airline_right_alt_sep = '<'

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
let g:airline#extensions#tabline#fnamemod = ':t'

let g:hardtime_default_on = 1

let g:fzf_layout = { 'down': '~20%' }
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }
