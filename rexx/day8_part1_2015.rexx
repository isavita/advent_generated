
/* REXX */
call main
exit

main:
  total_diff = 0
  filename = 'input.txt'

  do while lines(filename) > 0
    line = linein(filename)
    diff = 2
    content = substr(line, 2, length(line) - 2)
    i = 1
    do while i <= length(content)
      if substr(content, i, 1) = '\' then do
        if substr(content, i + 1, 1) = 'x' then do
          diff = diff + 3
          i = i + 4
        end
        else do
          diff = diff + 1
          i = i + 2
        end
      end
      else
        i = i + 1
    end
    total_diff = total_diff + diff
  end

  say total_diff
return
