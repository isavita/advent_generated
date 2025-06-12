
/* Rexx */
main:
  hx = 0
  hy = 0
  tx = 0
  ty = 0
  visited. = 0
  visited.0.0 = 1
  count = 1
  file = 'input.txt'

  do while lines(file) > 0
    parse value linein(file) with dir steps
    do i = 1 to steps
      select
        when dir = 'R' then hx = hx + 1
        when dir = 'L' then hx = hx - 1
        when dir = 'U' then hy = hy + 1
        when dir = 'D' then hy = hy - 1
      end

      dx = hx - tx
      dy = hy - ty

      if abs(dx) > 1 | abs(dy) > 1 then do
        tx = tx + sign(dx)
        ty = ty + sign(dy)
        if visited.tx.ty = 0 then do
          visited.tx.ty = 1
          count = count + 1
        end
      end
    end
  end

  say count
  call lineout file
return
