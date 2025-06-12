
call main
exit

main:
  cubes. = 0
  list.0 = 0
  fname = 'input.txt'

  do while lines(fname) > 0
    line = linein(fname)
    parse var line x ',' y ',' z
    cubes.x.y.z = 1
    list.0 = list.0 + 1
    i = list.0
    list.i.1 = x
    list.i.2 = y
    list.i.3 = z
  end
  call linein fname, 1, 0

  surfaceArea = 0
  dirs = '1 0 0 -1 0 0 0 1 0 0 -1 0 0 0 1 0 0 -1'

  do i = 1 to list.0
    x = list.i.1
    y = list.i.2
    z = list.i.3
    do j = 1 to 6
      p = (j - 1) * 3 + 1
      nx = x + word(dirs, p)
      ny = y + word(dirs, p + 1)
      nz = z + word(dirs, p + 2)
      if cubes.nx.ny.nz = 0 then
        surfaceArea = surfaceArea + 1
    end
  end

  say surfaceArea
  return
