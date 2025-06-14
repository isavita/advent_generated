
/* Rexx */
call main
exit

main:
  fileName = 'input.txt'
  count = 0
  do while lines(fileName) > 0
    count = count + 1
    lines.count = linein(fileName)
  end
  call stream fileName, 'C', 'CLOSE'
  lines.0 = count

  do i = 1 to lines.0 - 1
    do j = i + 1 to lines.0
      diff = 0
      diff_pos = 0
      do k = 1 to length(lines.i)
        if substr(lines.i, k, 1) <> substr(lines.j, k, 1) then do
          diff = diff + 1
          diff_pos = k
        end
        if diff > 1 then leave k
      end

      if diff = 1 then do
        say delstr(lines.i, diff_pos, 1)
        exit
      end
    end
  end
return
