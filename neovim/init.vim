call plug#begin('~/.nvim/plugged')

let g:python3_host_prog = '/usr/local/bin/python3.6'
" essential
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'godlygeek/tabular'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'autozimu/LanguageClient-neovim', { 'do': ':UpdateRemotePlugins' }

" colors
Plug 'raichoo/monodark'

" airline
Plug 'vim-airline/vim-airline'

" languages
Plug 'raichoo/smt-vim'
Plug 'pbrisbin/vim-syntax-shakespeare'
Plug 'neovimhaskell/haskell-vim'
Plug 'dag/vim-fish'
Plug 'rust-lang/rust.vim'
Plug 'derekelkins/agda-vim'

" git
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

call plug#end()

source ~/.config/nvim/haskell.vim

filetype plugin indent on

function! Rename(file) abort
  let l:f = expand('%')
  execute 'saveas ' . shellquote(a:file) . ' | ' . 'bd! # | !rm ' . shellquote(l:f)
endfunction
command! -complete=file -nargs=1 Rename call Rename(<f-args>)

function! FixWhitespaces() abort
  let l:search = @/
  let l:l = line('.')
  let l:c = col('.')

  %s/\s\+$//e

  let @/ = l:search
  call cursor(l:l, l:c)
endfunction
command! FixWhitespaces call FixWhitespaces()

function! HighlightSearch(word) abort
  let l:w = expand('<cword>')

  if a:word
    let @/ = '\<' . l:w . '\>'
  else
    let @/ = l:w
  endif
endfunction

let mapleader="ö"

if isdirectory('.git')
  call denite#custom#var('file_rec', 'command',
  \ ['git', 'ls-files', '--others', '--cached', '--exclude-standard'])
endif

map <silent> <Leader>ls :Denite buffer<CR>
map <silent> <Leader>lf :Denite file_rec<CR>

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

map Q <nop>
map gQ <nop>

inoremap <C-l> <Esc>

nnoremap <silent> * :call HighlightSearch(1)<CR>:let v:searchforward=1<CR>:set hlsearch<CR>
nnoremap <silent> # :call HighlightSearch(1)<CR>:let v:searchforward=0<CR>:set hlsearch<CR>
nnoremap <silent> g* :call HighlightSearch(0)<CR>:let v:searchforward=1<CR>:set hlsearch<CR>
nnoremap <silent> g# :call HighlightSearch(0)<CR>:let v:searchforward=0<CR>:set hlsearch<CR>
vnoremap <silent> * :call HighlightSearch(1)<CR>:let v:searchforward=1<CR>:set hlsearch<CR>
vnoremap <silent> # :call HighlightSearch(1)<CR>:let v:searchforward=0<CR>:set hlsearch<CR>
vnoremap <silent> g* :call HighlightSearch(0)<CR>:let v:searchforward=1<CR>:set hlsearch<CR>
vnoremap <silent> g# :call HighlightSearch(0)<CR>:let v:searchforward=1<CR>:set hlsearch<CR>

nnoremap <silent> <C-l> :noh<CR>

colorscheme monodark

set formatoptions=jcroql
set termguicolors
set guicursor=a:block-blinkon100-Cursor/Cursor
set cpo-=_
set nojoinspaces
set splitright
set inccommand=nosplit
set noerrorbells
set novisualbell
set visualbell t_bv=
set clipboard=unnamed,unnamedplus
set mouse=
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
set cursorline
set undofile
set undodir=~/.nvim/tmp/undo/
set backupdir=~/.nvim/tmp/backup/
set directory=~/.nvim/tmp/swap/
set backup
set noswapfile
set list
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮,nbsp:⊙
set wrap
set textwidth=80
set shortmess+=I
set grepprg=rg\ --vimgrep\ --no-heading
set grepformat=%f:%l:%c:%m,%f:%l:%m

augroup commands
  au!
  au InsertEnter,WinEnter * set nocursorline
  au InsertLeave,WinEnter * set cursorline
  au TermOpen * setlocal nonumber norelativenumber
  au BufNewFile,BufRead *.d setf dtrace
  au BufNewFile,BufRead *.c,*.h setlocal tabstop=8 shiftwidth=4 softtabstop=4 noexpandtab
  au BufNewFile,BufRead *.agda setf agda
augroup end

let g:agda_extraincpaths = ["/usr/home/raichoo/Sources/agda-stdlib-0.14/src"]

let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_fastbrowse = 0
let g:netrw_bufsettings = "noma nomod nu nobl nowrap ro rnu"

let g:haskell_enable_quantification = 1
let g:haskell_enable_typeroles = 1
let g:haskell_enable_pattern_synonyms = 1
let g:haskell_indent_case_alternative = 1

let g:hamlet_prevent_invalid_nesting = 0

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

let g:deoplete#enable_at_startup = 1

let g:LanguageClient_changeThrottle = 0.5

function! denite#get_status_mode() abort
  return ''
endfunction

call denite#custom#map(
      \ 'insert',
      \ '<C-n>',
      \ '<denite:move_to_next_line>',
      \ 'noremap'
      \)
call denite#custom#map(
      \ 'insert',
      \ '<C-p>',
      \ '<denite:move_to_previous_line>',
      \ 'noremap'
      \)
