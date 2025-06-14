
/* Rexx */
call main
exit

main:
  b = 57*100 + 100000
  c = b + 17000
  h = 0
  do x = b to c by 17
    if isPrime(x) = 0 then
      h = h + 1
  end
  say h
return

isPrime: procedure
  arg n
  do i = 2 while i*i <= n
    if n // i = 0 then
      return 0
  end
  return 1
