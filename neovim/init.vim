let g:python3_host_prog = '/usr/local/bin/python3.6'

filetype plugin indent on

colorscheme monodark

set path=.,**
set formatoptions=jcroql
set termguicolors
set guicursor=a:block-blinkon100-Cursor/Cursor
set cpo-=_
set nojoinspaces
set splitright
set inccommand=nosplit
set clipboard=unnamed,unnamedplus
set hidden
set linebreak
set noshowmode
set shiftwidth=2
set tabstop=2
set relativenumber number
set expandtab
set cmdheight=1
set cursorline
set undofile
set undodir=~/.config/nvim/tmp/undo/
set backupdir=~/.config/nvim/tmp/backup/
set directory=~/.config/nvim/tmp/swap/
set backup
set noswapfile
set list
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮,nbsp:▩
set wrap
set textwidth=80
set shortmess+=I
set grepprg=rg\ --vimgrep\ --no-heading
set grepformat=%f:%l:%c:%m,%f:%l:%m

function! FixWhitespaces() abort
  let l:search = @/
  let l:l = line('.')
  let l:c = col('.')
  %s/\(\s\+\|\s*\+\)$//e
  let @/ = l:search
  call statusline#BufferState()
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

map  <Leader>ls :ls<CR>:b
map  <Leader>lf :find *

map <silent> [a :prev<CR>
map <silent> ]a :next<CR>
map <silent> [A :first<CR>
map <silent> ]A :last<CR>

map <silent> [c :cprev<CR>
map <silent> ]c :cnext<CR>
map <silent> [C :cfirst<CR>
map <silent> ]C :clast<CR>

map <silent> [l :lprev<CR>
map <silent> ]l :lnext<CR>
map <silent> [L :lfirst<CR>
map <silent> ]L :llast<CR>

map Y y$
map <silent> & :&&<CR>
vmap <silent> & :'<,'>&&<CR>

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
map Q <nop>
map gQ <nop>
inoremap <Up> <nop>
inoremap <Down> <nop>
inoremap <Left> <nop>
inoremap <Right> <nop>
inoremap <PageUp> <nop>
inoremap <PageDown> <nop>

function! ClearTerminal() abort
  if exists('b:terminal_job_id')
    if confirm("Clear terminal?", "&Yes\n&No", 2) == 1
      call chansend(b:terminal_job_id, '')
    endif
  endif
endfunction

nnoremap <silent> <C-l> :noh<CR>:call ClearTerminal()<CR>
inoremap <C-l> <Esc>
tnoremap <C-l> <C-\><C-n>

nnoremap <silent> * :call HighlightSearch(1)<CR>:let v:searchforward=1<CR>:set hls<CR>
nnoremap <silent> # :call HighlightSearch(1)<CR>:let v:searchforward=0<CR>:set hls<CR>
nnoremap <silent> g* :call HighlightSearch(0)<CR>:let v:searchforward=1<CR>:set hls<CR>
nnoremap <silent> g# :call HighlightSearch(0)<CR>:let v:searchforward=0<CR>:set hls<CR>
vnoremap <silent> * :call HighlightSearch(1)<CR>:let v:searchforward=1<CR>:set hls<CR>
vnoremap <silent> # :call HighlightSearch(1)<CR>:let v:searchforward=0<CR>:set hls<CR>
vnoremap <silent> g* :call HighlightSearch(0)<CR>:let v:searchforward=1<CR>:set hls<CR>
vnoremap <silent> g# :call HighlightSearch(0)<CR>:let v:searchforward=1<CR>:set hls<CR>

let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_fastbrowse = 0
let g:netrw_bufsettings = "noma nomod nu nobl nowrap ro rnu"

let g:haskell_enable_quantification = 1
let g:haskell_enable_typeroles = 1
let g:haskell_enable_pattern_synonyms = 1
let g:haskell_indent_case_alternative = 1

let g:hamlet_prevent_invalid_nesting = 0

augroup commands
  au!
  au InsertEnter,WinEnter * set nocursorline
  au InsertLeave,WinEnter * set cursorline
  au BufEnter *.d setf dtrace
  au TermOpen * setlocal nonumber
augroup end
