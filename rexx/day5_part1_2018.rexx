
/* Rexx */
call main
exit

main:
  polymer = linein('input.txt')
  result = ''
  do i = 1 to length(polymer)
    char = substr(polymer, i, 1)
    if result \== '' & right(result, 1) \== char &,
       translate(right(result, 1)) == translate(char) then
      result = substr(result, 1, length(result) - 1)
    else
      result = result || char
  end
  say length(result)
return
