
/* Rexx */
call main
exit

main:
  x = 1
  cycle = 0
  crt_output = ''
  fileName = 'input.txt'

  do while lines(fileName) > 0
    line = linein(fileName)
    parse var line instruction value

    if instruction = 'noop' then do
      call draw_pixel
      cycle = cycle + 1
    end
    else if instruction = 'addx' then do
      call draw_pixel
      cycle = cycle + 1
      call draw_pixel
      cycle = cycle + 1
      x = x + value
    end
  end

  do row = 0 to 5
    say substr(crt_output, row * 40 + 1, 40)
  end
return

draw_pixel:
  crt_x = cycle // 40
  if abs(crt_x - x) <= 1 then
    crt_output = crt_output || '#'
  else
    crt_output = crt_output || '.'
return
