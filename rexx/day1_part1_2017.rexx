
/* REXX */
signal on syntax name syntax_error
call main
exit

main:
  data = strip(linein('input.txt'))
  total = 0
  len = length(data)
  do i = 1 to len
    next_i = (i // len) + 1
    if substr(data, i, 1) = substr(data, next_i, 1) then
      total = total + substr(data, i, 1)
  end
  say total
  return

syntax_error:
  say 'Error:' errortext(rc) 'at line' sigl
  exit 1
