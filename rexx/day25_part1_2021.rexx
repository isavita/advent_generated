
/* Rexx */
main:
  call ReadInput 'input.txt'
  do steps = 1 by 1
    if \Step() then do
      say steps
      leave
    end
  end
return

Step: procedure expose grid. width height
  moved = 0
  do i = 1 to height; temp_grid.i = grid.i; end
  do y = 1 to height
    do x = 1 to width
      if substr(grid.y, x, 1) = '>' then do
        next_x = x // width + 1
        if substr(grid.y, next_x, 1) = '.' then do
          temp_grid.y = overlay('.', temp_grid.y, x)
          temp_grid.y = overlay('>', temp_grid.y, next_x)
          moved = 1
        end
      end
    end
  end
  do i = 1 to height; grid.i = temp_grid.i; end

  do i = 1 to height; temp_grid.i = grid.i; end
  do y = 1 to height
    do x = 1 to width
      if substr(grid.y, x, 1) = 'v' then do
        next_y = y // height + 1
        if substr(grid.next_y, x, 1) = '.' then do
          temp_grid.y = overlay('.', temp_grid.y, x)
          temp_grid.next_y = overlay('v', temp_grid.next_y, x)
          moved = 1
        end
      end
    end
  end
  do i = 1 to height; grid.i = temp_grid.i; end
  return moved

ReadInput: procedure expose grid. width height
  parse arg fname
  y = 0
  do while lines(fname) > 0
    y = y + 1
    grid.y = linein(fname)
  end
  call lineout fname
  height = y
  if height > 0 then width = length(grid.1)
  else width = 0
return
