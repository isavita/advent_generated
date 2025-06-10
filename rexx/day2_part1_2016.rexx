
/* Rexx */
call main
exit

main:
  x = 1
  y = 1
  code = ''
  fileName = 'input.txt'

  do while lines(fileName) > 0
    line = linein(fileName)
    do i = 1 to length(line)
      char = substr(line, i, 1)
      select
        when char = 'U' then y = max(0, y - 1)
        when char = 'D' then y = min(2, y + 1)
        when char = 'L' then x = max(0, x - 1)
        when char = 'R' then x = min(2, x + 1)
        otherwise nop
      end
    end
    code = code || (y * 3 + x + 1)
  end

  call stream fileName, 'C', 'CLOSE'
  say code
return
