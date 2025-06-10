
/* Rexx */
call main
exit

main:
  possible_triangles = 0
  fileName = 'input.txt'

  do while lines(fileName) > 0
    line1 = linein(fileName)
    line2 = linein(fileName)
    line3 = linein(fileName)

    do j = 1 to 3
      a = word(line1, j)
      b = word(line2, j)
      c = word(line3, j)

      s1 = min(a, b, c)
      s3 = max(a, b, c)
      s2 = (a + b + c) - s1 - s3

      if (s1 + s2) > s3 then
        possible_triangles = possible_triangles + 1
    end
  end

  say possible_triangles
  return
