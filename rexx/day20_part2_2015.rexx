
/* REXX */
call main
exit

main:
  target = linein('input.txt') % 11
  call stream 'input.txt', 'c', 'close'

  houses. = 0

  do elf = 1 to target
    limit = min(elf * 50, target)
    do house = elf to limit by elf
      houses.house = houses.house + elf
    end
  end

  do house_number = 1 to target
    if houses.house_number >= target then do
      say house_number
      leave
    end
  end
return
