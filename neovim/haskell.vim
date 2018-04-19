if exists("g:loaded_haskell_init")
  finish
endif

let g:loaded_haskell_init = 1

let g:haskell_enable_quantification = 1
let g:haskell_enable_typeroles = 1
let g:haskell_enable_pattern_synonyms = 1
let g:haskell_indent_case_alternative = 1

let g:hamlet_prevent_invalid_nesting = 0

if executable('hasktags')
  function! s:HaskellRebuildTagsFinished(job_id, data, event) abort
    let g:haskell_rebuild_tags = 0
  endfunction
  let s:HaskellRebuildTagsFinishedHandler = {
    \ 'on_exit': function('s:HaskellRebuildTagsFinished')
    \ }

  function! s:HaskellRebuildTags() abort
    if !get(g:, 'haskell_rebuild_tags', 0)
      let l:cmd = 'hasktags --ignore-close-implementation --ctags .; sort tags'
      let g:haskell_rebuild_tags = jobstart(l:cmd, s:HaskellRebuildTagsFinishedHandler)
    endif
  endfunction

  augroup haskell_tags
    au!
    au BufWritePost *.hs call s:HaskellRebuildTags()
  augroup end

  command! HaskTags call s:HaskellRebuildTags()
endif

function! s:HaskellHealth(state, resolver)
  let l:health = ''

  if a:state is# 'ide'
    let l:health = '%#HaskellReadyLTS#●' . a:resolver
  elseif a:state is# 'ready'
    let l:health = '%#HaskellReadyLTS#' . a:resolver
  elseif a:state is# 'initialized'
    let l:health = '%#HaskellInitLTS#' . a:resolver
  elseif a:state is# 'uninitialized'
    let l:health = '%#HaskellUninitLTS#' . a:resolver
  elseif a:state is# 'missing'
    let l:health = a:resolver
  endif
  let g:airline_section_x = airline#section#create(['filetype', ' ', l:health])
  AirlineRefresh
endfunction

let g:haskell_supported_extensions = []

let g:haskell_supported_pragmas = [
  \ 'COLUMN',
  \ 'COMPLETE',
  \ 'DEPRECATED',
  \ 'INCOHERENT',
  \ 'INLINABLE',
  \ 'INLINE',
  \ 'INLINE CONLIKE',
  \ 'LANGUAGE',
  \ 'LINE',
  \ 'MINIMAL',
  \ 'NOLINE',
  \ 'NOINLINE CONLIKE',
  \ 'NOUNPACK',
  \ 'OPTIONS_GHC',
  \ 'OVERLAPPABLE',
  \ 'OVERLAPPING',
  \ 'OVERLAPS',
  \ 'RULES',
  \ 'SOURCE',
  \ 'SPECIALIZE',
  \ 'SPECIALIZE INLINE',
  \ 'UNPACK',
  \ 'WARNING']

let g:haskell_supported_keywords = [
  \ 'as',
  \ 'capi',
  \ 'case',
  \ 'ccall',
  \ 'class',
  \ 'data',
  \ 'data family',
  \ 'data instance',
  \ 'default',
  \ 'deriving',
  \ 'deriving anyclass',
  \ 'deriving instance',
  \ 'deriving newtype',
  \ 'deriving stock',
  \ 'export',
  \ 'forall',
  \ 'foreign',
  \ 'foreign import',
  \ 'hiding',
  \ 'import',
  \ 'import qualified',
  \ 'in',
  \ 'infix',
  \ 'infixl',
  \ 'infixr',
  \ 'instance',
  \ 'interruptible',
  \ 'let',
  \ 'mdo',
  \ 'module',
  \ 'newtype',
  \ 'of',
  \ 'pattern',
  \ 'prim',
  \ 'proc',
  \ 'qualified',
  \ 'rec',
  \ 'safe',
  \ 'static',
  \ 'type',
  \ 'type family',
  \ 'type instance',
  \ 'type role',
  \ 'unsafe',
  \ 'where']

function! HaskellComplete(findstart, base)
  if a:findstart
    let line = getline('.')
    let start = col('.') - 1
    while start > 0 && line[start - 1] =~ '\a'
      let start -= 1
    endwhile
    return start
  else
    let res = []
    let l:line = getline('.')
    if l:line =~ '^{-#\s\+LANGUAGE'
      for m in g:haskell_supported_extensions
        if m =~ '^' . a:base
          call add(res, m)
        endif
      endfor
    elseif l:line =~ '^{-#\s\+$'
      for m in g:haskell_supported_pragmas
        if m =~ '^' . a:base
          call add(res, m)
        endif
      endfor
    else
      for m in g:haskell_supported_keywords
        if m =~ '^' . a:base
          call add(res, m)
        endif
      endfor
    endif
    return res
  endif
endfunction
set omnifunc=HaskellComplete

function! s:HaskellSetup(...) abort
  highlight HaskellUninitLTS guifg=#EF5939 guibg=#465457
  highlight HaskellReadyLTS  guifg=#B8E673 guibg=#465457
  highlight HaskellInitLTS   guifg=#E6DB74 guibg=#465457

  let g:haskell_original_path = get(g:, 'haskell_original_path', $PATH)
  let g:haskell_supported_extensions = []

  function! s:HaskellRegisterExtensions(job_id, data, event) abort
    for ext in a:data
      if ext isnot# ''
        call add(g:haskell_supported_extensions, ext)
      endif
    endfor
  endfunction
  let s:HaskellRegisterExtensionsHandler = {
    \ 'on_stdout': function('s:HaskellRegisterExtensions')
    \ }

  function! s:HaskellSetupEnv() abort
    call s:HaskellHealth('ready', get(g:, 'haskell_resolver', '[unknown]'))

    if &filetype is# 'haskell'
      call s:HaskellSettings()
    endif
  endfunction

  function! s:HaskellPackagePath(job_id, data, event) abort
    let l:path = a:data[0]

    if l:path isnot# ''
      let $GHC_PACKAGE_PATH = l:path
      call s:HaskellHealth('initialized', get(g:, 'haskell_resolver', '[unknown]'))

      call s:HaskellSetupEnv()
    endif
  endfunction
  let s:HaskellPackagePathHandler = {
   \ 'on_stdout': function('s:HaskellPackagePath')
   \ }

  function! s:HaskellPath(job_id, data, event) abort
    let l:path = a:data[0]

    if l:path isnot# ''
      let l:lts_prefix = matchstr(get(g:, 'haskell_resolver'), '^[^.]*')
      if l:lts_prefix isnot# ''
        let l:envpath = $HOME . '/Local/ghc/' . l:lts_prefix . '/bin'
        let $PATH = l:envpath . ':' . join(filter(split(l:path, ':'), 'v:val isnot# "' . l:envpath . '"'), ':')

        call jobstart('ghc --supported-extensions', s:HaskellRegisterExtensionsHandler)
        call jobstart('env PATH=' . l:envpath . ':' . g:haskell_original_path . ' stack --no-install-ghc exec printenv GHC_PACKAGE_PATH', s:HaskellPackagePathHandler)
      else
        let $PATH = l:path

        call jobstart('ghc --supported-extensions', s:HaskellRegisterExtensionsHandler)
        call jobstart('env PATH=' . g:haskell_original_path . ' stack --no-install-ghc exec printenv GHC_PACKAGE_PATH', s:HaskellPackagePathHandler)
      endif
    endif
  endfunction
  let s:HaskellPathHandler = {
   \ 'on_stdout': function('s:HaskellPath')
   \ }

  if a:0
    let l:envpath = $HOME . '/Local/ghc/' . a:1 . '/bin'
    call s:HaskellHealth('missing', a:1)

    if isdirectory(l:envpath)
      let $PATH = l:envpath . ':' . g:haskell_original_path
      call s:HaskellHealth('ready', a:1)
      call jobstart('ghc --supported-extensions', s:HaskellRegisterExtensionsHandler)
    endif
  else
    let l:resolver = systemlist('grep "^resolver:" stack.yaml | cut -d" " -f2')[0]

    let g:haskell_resolver = l:resolver
    let l:lts_prefix = matchstr(l:resolver, '^[^.]*')
    let l:envpath = $HOME . '/Local/ghc/' . l:lts_prefix . '/bin'
    if isdirectory(l:envpath)
      let $PATH = l:envpath . ':' . g:haskell_original_path

      if l:lts_prefix isnot# '' && isdirectory(l:envpath)
        call s:HaskellHealth('uninitialized', get(g:, 'haskell_resolver', '[unknown]'))
        if isdirectory($HOME . '/.stack/snapshots/x86_64-freebsd/' . g:haskell_resolver)
          call jobstart('env PATH=' . l:envpath . ':' . g:haskell_original_path . ' stack --no-install-ghc exec printenv PATH', s:HaskellPathHandler)
        endif
      endif
    else
      call s:HaskellHealth('missing', l:resolver)
    endif
  endif
endfunction
function! s:HaskellEnvs(lead, line, pos) abort
  return system("find ~/Local/ghc -depth 1 -exec basename '{}' + | sort")
endfunction
command! -complete=custom,s:HaskellEnvs -nargs=? HaskEnv call s:HaskellSetup(<f-args>)

function! s:HaskellSkel() abort
  if @% is# 'Main.hs'
    silent! normal! imodule Main wheremain :: IO ()main = return ()2B
  else
    silent! normal! "%p
    silent! s/\v^%([0-9a-z].{-}\/)*(.{-})\.hs/module \u\1 where/
    silent! s/\//./g
    silent! normal!o
  endif
endfunction

function! s:HaskellSettings() abort
  setlocal suffixesadd+=.hs,.hamlet

  if executable('stylish-haskell')
    setlocal formatprg=stylish-haskell
  endif

  if executable('hoogle')
    setlocal keywordprg=hoogle\ --info
  endif
endfunction

augroup haskell_commands
  au!
  au BufNewFile *.hs call s:HaskellSkel() | call s:HaskellSettings()
  au BufRead *.hs call s:HaskellSettings()
  au BufNewFile,BufRead *.dump-stg,*.dump-simpl setf haskell
  au BufNewFile,BufRead *.dump-cmm,*.dump-opt-cmm setf c
  au BufNewFile,BufRead *.dump-asm setf asm

  if filereadable('stack.yaml')
    au VimEnter * call s:HaskellSetup()
    au BufWritePost stack.yaml call s:HaskellSetup()
  endif
augroup end
