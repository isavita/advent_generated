
/* Rexx */
call main
exit

main:
  count = 0
  fileName = 'input.txt'
  do while lines(fileName) > 0
    line = linein(fileName)
    parse var line l1 '-' l2 ',' r1 '-' r2
    if l1 <= r2 & l2 >= r1 then
      count = count + 1
  end
  say count
return
