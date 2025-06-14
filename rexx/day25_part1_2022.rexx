
/* REXX */
main:
  numeric digits 30
  sum = 0
  fileName = 'input.txt'
  do while lines(fileName) > 0
     sum = sum + fromSnafu(linein(fileName))
  end
  call stream fileName, 'c', 'close'
  say toSnafu(sum)
exit

fromSnafu: procedure
  arg s
  n = 0
  map = '=-012'
  do i = 1 to length(s)
    n = n * 5 + pos(substr(s, i, 1), map) - 3
  end
  return n

toSnafu: procedure
  arg n
  if n = 0 then return '0'
  res = ''
  map = '=-012'
  do while n > 0
    rem = (n + 2) // 5
    res = substr(map, rem + 1, 1) || res
    n = (n + 2) % 5
  end
  return res
