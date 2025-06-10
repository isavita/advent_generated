
/* Rexx */
main:
  parse arg fn
  if fn = '' then fn = 'input.txt'
  target = 150
  count = 0
  i = 0
  do while lines(fn) > 0
    i = i + 1
    c.i = linein(fn)
  end
  c.0 = i
  call stream fn, 'c', 'close'

  call find_combinations 1, 0

  say count
exit

find_combinations: procedure expose count target c.
  parse arg start_index, current_sum
  do j = start_index to c.0
    new_sum = current_sum + c.j
    if new_sum = target then
      count = count + 1
    else if new_sum < target then
      call find_combinations j + 1, new_sum
  end
return
