
/* REXX */
call main
exit

main:
  numeric digits 30
  file = 'input.txt'
  x = 0
  y = 0
  shoelace_sum = 0
  perimeter = 0

  do while lines(file) > 0
    line = linein(file)
    parse var line . . hexcode

    len = x2d(substr(hexcode, 3, 5))
    dir = substr(hexcode, 8, 1)

    perimeter = perimeter + len

    prev_x = x
    prev_y = y

    select
      when dir = '0' then x = x + len /* R */
      when dir = '1' then y = y + len /* D */
      when dir = '2' then x = x - len /* L */
      when dir = '3' then y = y - len /* U */
    end

    shoelace_sum = shoelace_sum + (prev_x * y - prev_y * x)
  end
  call lineout file

  area = abs(shoelace_sum) % 2
  total_area = area + (perimeter % 2) + 1

  say total_area
  return
