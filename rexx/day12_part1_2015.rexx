
/* REXX */
call main
main:
  fileName = 'input.txt'
  call stream fileName, 'c', 'open read'
  jsonStr = charin(fileName, 1, chars(fileName))
  call stream fileName, 'c', 'close'

  nonNum = '[]{},:"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
  jsonStr = translate(jsonStr, copies(' ', length(nonNum)), nonNum)

  total = 0
  do i = 1 to words(jsonStr)
    w = word(jsonStr, i)
    if datatype(w, 'N') then total = total + w
  end

  say total
return
