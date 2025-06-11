
/* Rexx */
call main

main:
  points.0 = 0
  fileName = 'input.txt'

  do while lines(fileName) > 0
    line = linein(fileName)
    i = points.0 + 1
    points.0 = i
    parse var line points.i.x ',' points.i.y ',' points.i.z ',' points.i.t
  end

  numPoints = points.0

  do i = 1 to numPoints
    parent.i = i
  end

  do i = 1 to numPoints
    do j = i + 1 to numPoints
      dist = abs(points.i.x - points.j.x)
      dist = dist + abs(points.i.y - points.j.y)
      dist = dist + abs(points.i.z - points.j.z)
      dist = dist + abs(points.i.t - points.j.t)

      if dist <= 3 then
        call union i, j
    end
  end

  constellationCount = 0
  do i = 1 to numPoints
    if parent.i = i then
      constellationCount = constellationCount + 1
  end

  say constellationCount
  exit

find: procedure expose parent.
  parse arg x
  root = x
  do while parent.root \= root
    root = parent.root
  end
  do while parent.x \= root
    next_x = parent.x
    parent.x = root
    x = next_x
  end
  return root

union: procedure expose parent.
  parse arg x, y
  rootX = find(x)
  rootY = find(y)
  if rootX \= rootY then
    parent.rootX = rootY
  return
