function! s:HaskellRebuildTagsFinished(job_id, data, event) abort
  let g:haskell_rebuild_tags = 0
endfunction
let s:HaskellTagsHandler = {
  \ 'on_exit': function('s:HaskellRebuildTagsFinished')
  \ }

function! s:HaskellRebuildTags() abort
  if g:haskell_rebuild_tags == 0 && filereadable('stack.yaml')
    let l:cmd = 'hasktags --ignore-close-implementation --ctags .; sort tags'
    let g:haskell_rebuild_tags = jobstart(l:cmd, s:HaskellTagsHandler)
  endif
endfunction

function! s:HaskellTagsSetupHandler(job_id, data, event) abort
  let g:haskell_rebuild_tags = 0
  augroup haskell_tags
    au!
    au BufWritePost *.hs call s:HaskellRebuildTags()
  augroup end
  echomsg 'haskell: hasktags ready'
endfunction
let s:HaskellTagsSetupHandler = {
  \ 'on_exit': function('s:HaskellTagsSetupHandler')
  \ }

function! s:HaskellGhcMod(job_id, data, event) abort
    call deoplete#initialize()
    call deoplete#enable()
    echomsg 'haskell: ghc-mod ready'
    if !executable('hasktags')
      echomsg 'haskell: installing hasktags'
      call jobstart('stack build hasktags', s:HaskellTagsSetupHandler)
    else
      let g:haskell_rebuild_tags = 0
      augroup haskell_tags
        au!
        au BufWritePost *.hs call s:HaskellRebuildTags()
      augroup end
    endif
endfunction
let s:HaskellGhcModHandler = {
  \ 'on_exit': function('s:HaskellGhcMod')
  \ }

function! s:HaskellPackage(job_id, data, event) abort
  if a:event is# 'stdout'
    let $GHC_PACKAGE_PATH = a:data[0]
    if exepath('ghc-mod') is# expand('$HOME') . '/.local/bin/ghc-mod' || !executable('ghc-mod')
      echomsg 'haskell: installing ghc-mod'
      call jobstart('stack build ghc-mod', s:HaskellGhcModHandler)
    else
      call deoplete#initialize()
      call deoplete#enable()
    endif
  endif
endfunction
let s:HaskellPackageHandler = {
 \ 'on_stdout': function('s:HaskellPackage')
 \ }

function! s:HaskellEnv(job_id, data, event) abort
  if a:event is# 'stdout'
    let $PATH = a:data[0]
    call jobstart('stack exec printenv GHC_PACKAGE_PATH', s:HaskellPackageHandler)
  endif
endfunction
let s:HaskellEnvHandler = {
 \ 'on_stdout': function('s:HaskellEnv')
 \ }

function! s:HaskellSkel() abort
  if @% is# 'Main.hs'
    silent! normal! imodule Main wheremain :: IO ()main = return ()2B
  else
    silent! normal! "%p
    silent! :s/\v^%([0-9a-z].{-}\/)*(.{-})\.hs/module \1 where/
    silent! :s/\//./g
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

function! HaskellSetup() abort
  let $STACK_PROJECT_ROOT = $PWD
  call jobstart('stack exec printenv PATH', s:HaskellEnvHandler)
endfunction

augroup haskell_commands
  au!
  au BufNewFile *.hs call s:HaskellSkel() | call s:HaskellSettings()
  au BufRead *.hs call s:HaskellSettings()
  au BufNewFile,BufRead *.dump-stg,*.dump-simpl setf haskell
  au BufNewFile,BufRead *.dump-cmm,*.dump-opt-cmm setf c
  au BufNewFile,BufRead *.dump-asm setf asm
augroup end
