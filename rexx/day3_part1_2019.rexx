
/* REXX */
call main
exit

main:
  grid. = 0
  line1 = linein('input.txt')
  line2 = linein('input.txt')
  call stream 'input.txt', 'c', 'close'

  x = 0; y = 0
  line1 = line1 || ','
  do while line1 <> ''
    parse var line1 move ',' line1
    dir = substr(move, 1, 1)
    steps = substr(move, 2)
    do i = 1 to steps
      select
        when dir = 'U' then y = y + 1
        when dir = 'D' then y = y - 1
        when dir = 'L' then x = x - 1
        when dir = 'R' then x = x + 1
      end
      grid.x.y = 1
    end
  end

  x = 0; y = 0
  min_dist = 999999999
  line2 = line2 || ','
  do while line2 <> ''
    parse var line2 move ',' line2
    dir = substr(move, 1, 1)
    steps = substr(move, 2)
    do i = 1 to steps
      select
        when dir = 'U' then y = y + 1
        when dir = 'D' then y = y - 1
        when dir = 'L' then x = x - 1
        when dir = 'R' then x = x + 1
      end
      if grid.x.y = 1 then do
        dist = abs(x) + abs(y)
        min_dist = min(min_dist, dist)
      end
    end
  end

  say min_dist
return
