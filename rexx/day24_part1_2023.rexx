
/* REXX */
call main
exit

main:
  numeric digits 32
  min_b = 200000000000000
  max_b = 400000000000000
  call parseInput
  count = 0
  do i = 1 to points.0
    p1x = points.i.px; p1y = points.i.py
    v1x = points.i.vx; v1y = points.i.vy
    do j = 1 to i - 1
      p2x = points.j.px; p2y = points.j.py
      v2x = points.j.vx; v2y = points.j.vy

      det = v1x * v2y - v2x * v1y
      if det \= 0 then do
        t1 = (v2y * (p2x - p1x) - v2x * (p2y - p1y)) / det
        t2 = (v1y * (p2x - p1x) - v1x * (p2y - p1y)) / det

        if t1 >= 0 & t2 >= 0 then do
          ix = p1x + v1x * t1
          iy = p1y + v1y * t1
          if ix >= min_b & ix <= max_b & iy >= min_b & iy <= max_b then
            count = count + 1
        end
      end
    end
  end
  say count
return

parseInput:
  i = 0
  filename = 'input.txt'
  do while lines(filename) > 0
    i = i + 1
    line = linein(filename)
    parse var line p.x ',' p.y ',' p.z '@' v.x ',' v.y ',' v.z
    points.i.px = strip(p.x)
    points.i.py = strip(p.y)
    points.i.pz = strip(p.z)
    points.i.vx = strip(v.x)
    points.i.vy = strip(v.y)
    points.i.vz = strip(v.z)
  end
  call stream filename, 'c', 'close'
  points.0 = i
return
