" Dump what the syntax file does to the buffer, in two parts: for every line,
" the syntax group of each run of characters as `[group]text`, where a run with
" no syntax group at all gets an empty `[]`; and then what each group is linked
" to, which is what decides the colour a reader actually sees.
"
" Reads the syntax file from $CENTJES_VIM_DIR and writes the dump to
" $CENTJES_SYNTAX_DUMP.  Run this with `--clean` so that a centjes-vim
" installed elsewhere in 'runtimepath' cannot contribute rules of its own.

execute 'set runtimepath^=' . $CENTJES_VIM_DIR
syntax on
set filetype=centjes

let dump = []
for lnum in range(1, line('$'))
  let text = getline(lnum)
  let rendered = ''
  let previous = ''
  let col = 1
  while col <= len(text)
    let group = synIDattr(synID(lnum, col, 1), 'name')
    if group !=# previous
      let rendered .= '[' . group . ']'
      let previous = group
    endif
    let rendered .= text[col - 1]
    let col += 1
  endwhile
  call add(dump, rendered)
endfor

redir => listing
silent! highlight
redir END

let links = []
for line in split(listing, "\n")
  if line =~# '^centjes'
    " A group defined any way other than `highlight def link` prints something
    " other than a link, and is left as vim wrote it so the diff shows it.
    call add(links, substitute(substitute(line, '\s\+', ' ', 'g'),
          \ '^\(\S\+\) xxx links to \(\S\+\)$', '\1 -> \2', ''))
  endif
endfor

call writefile(dump + ['', '--- highlight links ---'] + sort(links), $CENTJES_SYNTAX_DUMP)
qall!
