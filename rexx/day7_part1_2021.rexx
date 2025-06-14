
/* REXX */
main:
  call stream 'input.txt', 'c', 'open read'
  line = linein('input.txt')
  call stream 'input.txt', 'c', 'close'

  line = changestr(',', line, ' ')
  count = 0
  do i = 1 to words(line)
    count = count + 1
    p.i = word(line, i)
  end
  p.0 = count

  do i = 1 to p.0 - 1
    do j = i + 1 to p.0
      if p.i > p.j then do
        temp = p.i
        p.i = p.j
        p.j = temp
      end
    end
  end

  median_idx = (p.0 + 1) % 2
  median_pos = p.median_idx

  total_fuel = 0
  do i = 1 to p.0
    total_fuel = total_fuel + abs(p.i - median_pos)
  end

  say total_fuel
exit
