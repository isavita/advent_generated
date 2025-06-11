
call main
main:
  cycle = 0
  x = 1
  sum = 0
  fname = 'input.txt'
  do while lines(fname) > 0
    parse value linein(fname) with instruction value
    if instruction = 'noop' then do
      cycle = cycle + 1
      if (cycle - 20) // 40 = 0 then sum = sum + cycle * x
    end
    else do
      cycle = cycle + 1
      if (cycle - 20) // 40 = 0 then sum = sum + cycle * x
      cycle = cycle + 1
      if (cycle - 20) // 40 = 0 then sum = sum + cycle * x
      x = x + value
    end
  end
  say sum
return
