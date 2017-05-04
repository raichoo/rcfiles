function! GetHLStack() abort
  echomsg "STACK: " . join(map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")'), ',')
endfunction
