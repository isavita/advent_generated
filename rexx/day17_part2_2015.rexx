
/* REXX */
main:
  target_total = 150
  part1_count = 0
  part2_count = 0
  min_used = 9999
  fname = 'input.txt'
  i = 0
  do while lines(fname) > 0
    i = i + 1
    c.i = linein(fname)
  end
  c.0 = i
  call stream fname, 'c', 'close'

  call find_combos target_total, 1, 0

  say part1_count
  say part2_count
exit

find_combos: procedure expose c. part1_count part2_count min_used
  parse arg target, start_idx, num_used

  if target = 0 then do
    part1_count = part1_count + 1
    if num_used < min_used then do
      min_used = num_used
      part2_count = 1
    end
    else if num_used = min_used then
      part2_count = part2_count + 1
    return
  end

  if target < 0 | start_idx > c.0 then return

  do i = start_idx to c.0
    call find_combos (target - c.i), (i + 1), (num_used + 1)
  end
return
