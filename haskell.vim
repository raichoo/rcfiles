" initialze haskell environment
function! s:HaskellSetup() abort
  if $STACK_PROJECT_ROOT is# ""
    let $STACK_PROJECT_ROOT = $PWD

    " Teardown
    function! s:HaskellCleanup()
      autocmd! haskell_cleanup
      augroup! haskell_cleanup

      delfunction s:HaskellDone
      delfunction s:HaskellTagsDone
      delfunction s:HaskellTags
      delfunction s:HaskellGhcModDone
      delfunction s:HaskellGhcMod
      delfunction s:HaskellPath

      unlet s:HaskellTagsHandler
      unlet s:HaskellGhcModHandler
      unlet s:HaskellPathHandler
    endfunction

    function! s:HaskellDone() abort
      augroup haskell_cleanup
        au!
        au InsertLeave *.hs call s:HaskellCleanup() | delfunction s:HaskellCleanup
      augroup end
    endfunction

    " Setup hasktags
    function! s:HaskellTagsDone(msg) abort
      " hasktags functions
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
      if a:msg
        echomsg 'haskell: hasktags ready'
      endif
      call s:HaskellDone()
    endfunction

    function! s:HaskellTags(job_id, data, event) abort
      call s:HaskellTagsDone(1)
    endfunction
    let s:HaskellTagsHandler = {
      \ 'on_exit': function('s:HaskellTags')
      \ }

    " Setup ghc-mod
    function! s:HaskellGhcModDone(msg) abort
      call deoplete#enable()
      if a:msg
        echomsg 'haskell: ghc-mod ready'
      endif
      if !executable('hasktags')
        echomsg 'haskell: installing hasktags'
        call jobstart('stack build hasktags', s:HaskellTagsHandler)
      else
        call s:HaskellTagsDone(0)
      endif
    endfunction!

    function! s:HaskellGhcMod(job_id, data, event) abort
      call s:HaskellGhcModDone(1)
    endfunction
    let s:HaskellGhcModHandler = {
      \ 'on_exit': function('s:HaskellGhcMod')
      \ }

    " Setup paths
    function! s:HaskellPath(job_id, data, event) abort
      let $PATH = a:data[0]
      let $GHC_PACKAGE_PATH = a:data[1]
      echomsg 'haskell: paths set'
      function! s:HaskellLazyLoad()
        autocmd! haskell_lazy_load
        augroup! haskell_lazy_load
        if !executable('ghc-mod') || exepath('ghc-mod') is# expand('$HOME') . '/.local/bin/ghc-mod'
          echomsg 'haskell: installing ghc-mod'
          call jobstart('stack build ghc-mod', s:HaskellGhcModHandler)
        else
          call s:HaskellGhcModDone(0)
        endif
      endfunction

      augroup haskell_lazy_load
        au!
        au InsertEnter *.hs call s:HaskellLazyLoad() | delfunction s:HaskellLazyLoad
      augroup end
    endfunction
    let s:HaskellPathHandler = {
     \ 'on_stdout': function('s:HaskellPath')
     \ }

    call jobstart('stack exec printenv PATH GHC_PACKAGE_PATH', s:HaskellPathHandler)
  endif
endfunction

" helper functions
function! s:HaskellSkel() abort
  if @% is# 'Main.hs'
    silent! normal! imodule Main wheremain :: IO ()main = return ()2B
  else
    silent! normal! "%p
    silent! s/\v^%([0-9a-z].{-}\/)*(.{-})\.hs/module \1 where/
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
  else
    let g:deoplete#enable_at_startup = 1
  endif
augroup end
