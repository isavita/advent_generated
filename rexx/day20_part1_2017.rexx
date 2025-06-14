
/* REXX */
call main

main:
  filename = 'input.txt'
  min_accel = 999999999
  closest_particle = -1

  do i = 0 while lines(filename) > 0
    line = linein(filename)
    parse var line . 'a=<' ax ',' ay ',' az '>'
    accel = abs(ax) + abs(ay) + abs(az)
    if accel < min_accel then do
      min_accel = accel
      closest_particle = i
    end
  end

  say closest_particle
  return
