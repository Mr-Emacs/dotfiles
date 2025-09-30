function! DuplicateLineKeepCol()
  let lnum = line('.')
  let cnum = col('.')
  normal! yyp
  let lnum = lnum + 1
  let lineText = getline(lnum)
  let maxcol = strlen(lineText) + 1
  if cnum > maxcol
    let cnum = maxcol
  endif
  call cursor(lnum, cnum)
endfunction

nnoremap <F5> :call DuplicateLineKeepCol()<CR>
