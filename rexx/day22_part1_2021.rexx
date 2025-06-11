
/* Rexx */
call main
exit

main:
  grid. = 0
  filename = 'input.txt'

  do while lines(filename) > 0
    line = linein(filename)
    if line = '' then iterate

    parse var line action ' x=' x1 '..' x2 ',y=' y1 '..' y2 ',z=' z1 '..' z2

    if x1 < -50 | x2 > 50 | y1 < -50 | y2 > 50 | z1 < -50 | z2 > 50 then,
      iterate

    state = (action = 'on')

    do x = x1 to x2
      do y = y1 to y2
        do z = z1 to z2
          grid.x.y.z = state
        end
      end
    end
  end
  call close filename

  onCubes = 0
  do x = -50 to 50
    do y = -50 to 50
      do z = -50 to 50
        onCubes = onCubes + grid.x.y.z
      end
    end
  end

  say onCubes
return
