
/* REXX */
call main
exit

main:
  favorite_number = linein('input.txt')
  call lineout 'input.txt'
  target_steps = 50

  queue. = ''
  q_head = 1
  q_tail = 1
  queue.1 = '1 1 0'

  visited. = 0
  visited.1.1 = 1
  reachable_count = 1

  do while q_head <= q_tail
    parse var queue.q_head x y steps
    q_head = q_head + 1

    if steps >= target_steps then iterate

    next_steps = steps + 1
    call explore x, y + 1, next_steps
    call explore x, y - 1, next_steps
    call explore x + 1, y, next_steps
    call explore x - 1, y, next_steps
  end

  say reachable_count
return

explore:
  procedure expose favorite_number q_tail queue. visited. reachable_count
  arg new_x, new_y, steps

  if visited.new_x.new_y then return
  if \is_valid_move(new_x, new_y) then return

  visited.new_x.new_y = 1
  reachable_count = reachable_count + 1
  q_tail = q_tail + 1
  queue.q_tail = new_x new_y steps
return

is_valid_move:
  procedure expose favorite_number
  arg x, y

  if x < 0 | y < 0 then return 0
  num = x*x + 3*x + 2*x*y + y + y*y + favorite_number
  return countstr('1', x2b(d2x(num))) // 2 == 0
