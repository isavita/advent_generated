
/* REXX */
call main
exit

main:
  Size = 10007
  pos = 2019
  fileName = 'input.txt'

  do while lines(fileName) > 0
    line = linein(fileName)
    select
      when line = 'deal into new stack' then
        pos = Size - 1 - pos

      when word(line, 1) = 'cut' then do
        n = word(line, 2)
        pos = (pos - n) // Size
        if pos < 0 then pos = pos + Size
      end

      when word(line, 1) = 'deal' then do
        n = word(line, 4)
        pos = (pos * n) // Size
      end
    end
  end

  call lineout fileName
  say pos
return
