
/* Rexx */
main:
  possible_triangles = 0
  fileName = 'input.txt'
  do while lines(fileName) > 0
    parse value linein(fileName) with s1 s2 s3
    sides.1 = s1
    sides.2 = s2
    sides.3 = s3
    call sort sides
    if sides.1 + sides.2 > sides.3 then
      possible_triangles = possible_triangles + 1
  end
  say possible_triangles
return

sort: procedure expose sides.
  do i = 1 to 2
    do j = i + 1 to 3
      if sides.i > sides.j then do
        temp = sides.i
        sides.i = sides.j
        sides.j = temp
      end
    end
  end
return
