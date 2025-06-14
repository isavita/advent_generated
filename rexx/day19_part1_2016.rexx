
/* REXX */
call main
exit

main:
  parse value linein('input.txt') with numElves
  p2 = 1
  do while p2 <= numElves / 2
    p2 = p2 * 2
  end
  winner = 2 * (numElves - p2) + 1
  say winner
return
