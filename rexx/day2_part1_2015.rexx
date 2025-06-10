
/* REXX */
call main
exit

main:
  total_paper = 0
  fileName = 'input.txt'
  do while lines(fileName) > 0
    parse value linein(fileName) with l 'x' w 'x' h
    s1 = l * w
    s2 = w * h
    s3 = h * l
    total_paper = total_paper + 2 * (s1 + s2 + s3) + min(s1, s2, s3)
  end
  call stream fileName, 'C', 'CLOSE'
  say total_paper
return
