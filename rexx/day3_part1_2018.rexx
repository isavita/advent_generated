
/* Rexx */
call main
exit

main:
  fabric. = 0
  total_overlap = 0
  file = 'input.txt'

  do while lines(file) > 0
    line = linein(file)
    parse var line '#' . ' @ ' left ',' top ': ' width 'x' height

    do i = top to top + height - 1
      do j = left to left + width - 1
        fabric.i.j = fabric.i.j + 1
        if fabric.i.j = 2 then
          total_overlap = total_overlap + 1
      end
    end
  end

  say total_overlap
return
