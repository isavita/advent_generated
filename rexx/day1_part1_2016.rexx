
/* Rexx */
main:
  dir_x.0 = 0;  dir_y.0 = 1
  dir_x.1 = 1;  dir_y.1 = 0
  dir_x.2 = 0;  dir_y.2 = -1
  dir_x.3 = -1; dir_y.3 = 0

  x = 0
  y = 0
  current_dir = 0

  line = linein('input.txt')

  do forever
    parse var line instr ', ' line
    if instr = '' then leave

    turn = substr(instr, 1, 1)
    dist = substr(instr, 2)

    if turn = 'R' then
      current_dir = (current_dir + 1) // 4
    else
      current_dir = (current_dir - 1 + 4) // 4

    x = x + dir_x.current_dir * dist
    y = y + dir_y.current_dir * dist
  end

  say abs(x) + abs(y)
exit
