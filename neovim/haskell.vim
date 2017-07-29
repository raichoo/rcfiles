if filereadable('stack.yaml') && executable('hasktags')
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

  command! Hasktags call s:HaskellRebuildTags()
endif

function! s:HaskellSetup() abort
  highlight HaskellUninitLTS guifg=#EF5939 guibg=#465457
  highlight HaskellReadyLTS  guifg=#B8E673 guibg=#465457
  highlight HaskellInitLTS   guifg=#E6DB74 guibg=#465457

  function! s:HaskellStackHealth(state)
    if a:state is# 'ready'
      let l:health = '%#HaskellReadyLTS#'
    elseif a:state is# 'installing'
      let l:health = '%#HaskellInitLTS#●'
    elseif a:state is# 'initialized'
      let l:health = '%#HaskellInitLTS#'
    elseif a:state is# 'unintialized'
      let l:health = '%#HaskellUninitLTS#'
    endif
    let g:airline_section_x = airline#section#create(['filetype', ' ', l:health . g:haskell_stack_resolver])
    AirlineRefresh
  endfunction

  function! s:HaskellGhcMod(job_id, data, event) abort
    call s:HaskellStackHealth('ready')
    call deoplete#enable()
  endfunction
  let s:HaskellGhcModHandler = {
    \ 'on_exit': function('s:HaskellGhcMod')
    \ }

  function! s:HaskellPackagePath(job_id, data, event) abort
    let $GHC_PACKAGE_PATH = a:data[0]
    if !executable('ghc-mod') || exepath('ghc-mod') is# expand('$HOME') . '/.local/bin/ghc-mod'
      function! s:HaskellLazyLoad()
        autocmd! haskell_lazy_load
        augroup! haskell_lazy_load
          call s:HaskellStackHealth('installing')
          call jobstart('stack build ghc-mod', s:HaskellGhcModHandler)
      endfunction

      call s:HaskellStackHealth('initialized')
      augroup haskell_lazy_load
        au!
        au InsertEnter *.hs call s:HaskellLazyLoad() | delfunction s:HaskellLazyLoad
      augroup end
    else
      call s:HaskellStackHealth('ready')
      call deoplete#enable()
    endif
  endfunction
  let s:HaskellPackagePathHandler = {
   \ 'on_stdout': function('s:HaskellPackagePath')
   \ }

  function! s:HaskellPath(job_id, data, event) abort
    let $PATH = a:data[0]
    call jobstart('env PATH=' . g:haskell_original_path . ' stack exec printenv GHC_PACKAGE_PATH', s:HaskellPackagePathHandler)
  endfunction
  let s:HaskellPathHandler = {
   \ 'on_stdout': function('s:HaskellPath')
   \ }

  let l:resolver = systemlist('grep "^resolver:" stack.yaml | cut -d" " -f2')[0]

  if l:resolver isnot# get(g:, 'haskell_stack_resolver', '')
    let g:haskell_stack_resolver = l:resolver
    if !isdirectory(expand('$HOME') . '/.stack/snapshots/x86_64-freebsd/' . g:haskell_stack_resolver)
      call s:HaskellStackHealth('unintialized')
    else
      let g:haskell_original_path = get(g:, 'haskell_original_path', $PATH)
      call jobstart('env PATH=' . g:haskell_original_path . ' stack exec printenv PATH', s:HaskellPathHandler)
    endif
  endif
endfunction
command! HaskellSetup call s:HaskellSetup()

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
  if executable('hoogle')
    setlocal keywordprg=hoogle\ --info
  endif
  if executable('stylish-haskell')
    setlocal formatprg=stylish-haskell
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
  else
    let g:deoplete#enable_at_startup = 1
  endif
augroup end
