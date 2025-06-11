
/* REXX */
call main
exit

main:
  fname = 'input.txt'
  height = 0
  do while lines(fname) > 0
    height = height + 1
    line = linein(fname)
    if height = 1 then width = length(line)
    do x = 1 to width
      grid.height.x = substr(line, x, 1)
    end
  end

  visible. = 0

  do y = 1 to height
    max_l = -1; max_r = -1
    do x = 1 to width
      if grid.y.x > max_l then do
        visible.y.x = 1
        max_l = grid.y.x
      end
      xr = width - x + 1
      if grid.y.xr > max_r then do
        visible.y.xr = 1
        max_r = grid.y.xr
      end
    end
  end

  do x = 1 to width
    max_t = -1; max_b = -1
    do y = 1 to height
      if grid.y.x > max_t then do
        visible.y.x = 1
        max_t = grid.y.x
      end
      yb = height - y + 1
      if grid.yb.x > max_b then do
        visible.yb.x = 1
        max_b = grid.yb.x
      end
    end
  end

  count = 0
  do y = 1 to height
    do x = 1 to width
      count = count + visible.y.x
    end
  end

  say count
  return
