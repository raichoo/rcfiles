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

  command! Hasktags call s:HaskellRebuildTags()
endif

function! s:HaskellSetup() abort
  highlight HaskellUninitLTS guifg=#EF5939 guibg=#465457
  highlight HaskellReadyLTS  guifg=#B8E673 guibg=#465457
  highlight HaskellInitLTS   guifg=#E6DB74 guibg=#465457

  if $STACK_PROJECT_ROOT is# ""
    function! s:HaskellGhcMod(job_id, data, event) abort
      let g:airline_section_x = airline#section#create(['filetype', ' ', '%#HaskellReadyLTS#' . g:haskell_stack_resolver])
      AirlineRefresh
      call deoplete#enable()
    endfunction
    let s:HaskellGhcModHandler = {
      \ 'on_exit': function('s:HaskellGhcMod')
      \ }

    function! s:HaskellPath(job_id, data, event) abort
      let $PATH = a:data[0]
      let $GHC_PACKAGE_PATH = a:data[1]
      if !executable('ghc-mod') || exepath('ghc-mod') is# expand('$HOME') . '/.local/bin/ghc-mod'
        function! s:HaskellLazyLoad()
          autocmd! haskell_lazy_load
          augroup! haskell_lazy_load
            let g:airline_section_x = airline#section#create(['filetype', ' ', '%#HaskellInitLTS#●' . g:haskell_stack_resolver])
            AirlineRefresh
            call jobstart('stack build ghc-mod', s:HaskellGhcModHandler)
        endfunction

        let g:airline_section_x = airline#section#create(['filetype', ' ', '%#HaskellInitLTS#' . g:haskell_stack_resolver])
        AirlineRefresh
        augroup haskell_lazy_load
          au!
          au InsertEnter *.hs call s:HaskellLazyLoad() | delfunction s:HaskellLazyLoad
        augroup end
      else
        let g:airline_section_x = airline#section#create(['filetype', ' ', '%#HaskellReadyLTS#' . g:haskell_stack_resolver])
        AirlineRefresh
        call deoplete#enable()
      endif
    endfunction
    let s:HaskellPathHandler = {
     \ 'on_stdout': function('s:HaskellPath')
     \ }

    let g:haskell_stack_resolver = systemlist('grep "^resolver:" stack.yaml | cut -d" " -f2')[0]

    if !isdirectory(expand('$HOME') . '/.stack/snapshots/x86_64-linux/' . g:haskell_stack_resolver)
      let g:airline_section_x = airline#section#create(['filetype', ' ', '%#HaskellUninitLTS#' . g:haskell_stack_resolver])
      AirlineRefresh
    else
      let $STACK_PROJECT_ROOT = $PWD
      call jobstart('stack exec printenv PATH GHC_PACKAGE_PATH', s:HaskellPathHandler)
    endif
  endif
endfunction
command! HaskellSetup call s:HaskellSetup()

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
