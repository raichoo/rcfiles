let g:haskell_ide_state = ''

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

function! s:HaskellStackHealth(state)
  if a:state is# 'ready'
    let l:health = '%#HaskellReadyLTS#'
  elseif a:state is# 'missing'
    let l:health = '%#HaskellInitLTS#●'
  elseif a:state is# 'initialized'
    let l:health = '%#HaskellInitLTS#'
  elseif a:state is# 'unintialized'
    let l:health = '%#HaskellUninitLTS#'
  endif
  let g:haskell_ide_state = a:state
  let g:airline_section_x = airline#section#create(['filetype', ' ', l:health . g:haskell_stack_resolver])
  AirlineRefresh
endfunction

function! s:HaskellSetup() abort
  highlight HaskellUninitLTS guifg=#EF5939 guibg=#465457
  highlight HaskellReadyLTS  guifg=#B8E673 guibg=#465457
  highlight HaskellInitLTS   guifg=#E6DB74 guibg=#465457

  let g:haskell_original_path = get(g:, 'haskell_original_path', $PATH)

  if g:haskell_ide_state is# 'ready'
    let g:haskell_ide_state = ''
    if executable('hie')
      LanguageClientStop
    endif
  endif

  function! s:HaskellPackagePath(job_id, data, event) abort
    let l:path = a:data[0]

    if l:path isnot# ''
      let $GHC_PACKAGE_PATH = l:path

      if executable('hie')
        let g:LanguageClient_serverCommands = {
            \ 'haskell': ['hie', '--lsp'],
            \ }
        call s:HaskellStackHealth('initialized')

        if &filetype is# 'haskell'
          call s:HaskellSettings()
        endif
      else
        call s:HaskellStackHealth('missing')
      endif
    endif
  endfunction
  let s:HaskellPackagePathHandler = {
   \ 'on_stdout': function('s:HaskellPackagePath')
   \ }

  function! s:HaskellPath(job_id, data, event) abort
    let l:path = a:data[0]

    if l:path isnot# ''
      let $PATH = l:path

      call jobstart('env PATH=' . g:haskell_original_path . ' stack exec printenv GHC_PACKAGE_PATH', s:HaskellPackagePathHandler)
    endif
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
  if executable('stylish-haskell')
    setlocal formatprg=stylish-haskell
  endif

  if g:haskell_ide_state is# 'initialized'
    LanguageClientStart
    call s:HaskellStackHealth('ready')
  endif

  if g:haskell_ide_state is# 'ready'
    setlocal keywordprg=:call\ LanguageClient_textDocument_hover()
  elseif g:haskell_ide_state is# 'missing' && executable('hoogle')
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
