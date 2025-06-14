
/* Rexx */
call main
exit

main:
  call read_grid 'input.txt'
  total_sum = 0
  do y = 1 to grid.rows
    x = 1
    do while x <= grid.cols
      char = substr(grid.y, x, 1)
      if datatype(char, 'W') then do
        start_x = x
        end_x = start_x
        do j = start_x + 1 to grid.cols
          if \datatype(substr(grid.y, j, 1), 'W') then leave
          end_x = j
        end
        number = substr(grid.y, start_x, end_x - start_x + 1)
        if is_part_number(y, start_x, end_x) then
          total_sum = total_sum + number
        x = end_x + 1
      end
      else do
        x = x + 1
      end
    end
  end
  say total_sum
return

read_grid:
  procedure expose grid.
  filename = arg(1)
  grid. = ''
  y = 0
  do while lines(filename) > 0
    y = y + 1
    grid.y = linein(filename)
  end
  grid.rows = y
  if y > 0 then grid.cols = length(grid.1)
  else grid.cols = 0
  call lineout filename
return

is_part_number:
  procedure expose grid.
  parse arg y, x_start, x_end
  y_from = max(1, y - 1)
  y_to   = min(grid.rows, y + 1)
  x_from = max(1, x_start - 1)
  x_to   = min(grid.cols, x_end + 1)
  do j = y_from to y_to
    do i = x_from to x_to
      char = substr(grid.j, i, 1)
      if char \= '.' & \datatype(char, 'W') then
        return 1
    end
  end
  return 0
