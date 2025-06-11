
/* REXX */
call main
exit

main:
  sum = 0
  fileName = 'input.txt'
  alphabet = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'

  do while lines(fileName) > 0
    line = linein(fileName)
    half = length(line) % 2
    first = substr(line, 1, half)
    second = substr(line, half + 1)

    p = verify(second, first, 'M')
    if p > 0 then do
      item = substr(second, p, 1)
      sum = sum + pos(item, alphabet)
    end
  end

  say sum
return
