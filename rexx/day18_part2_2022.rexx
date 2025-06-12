
/* REXX */
main:
  parse arg file_in .
  if file_in = '' then file_in = 'input.txt'

  cubes. = 0
  seen. = 0

  d.1 = '-1 0 0'; d.2 = '1 0 0'
  d.3 = '0 -1 0'; d.4 = '0 1 0'
  d.5 = '0 0 -1'; d.6 = '0 0 1'

  min_x = 9999; min_y = 9999; min_z = 9999
  max_x = -9999; max_y = -9999; max_z = -9999

  do while lines(file_in) > 0
    line = strip(linein(file_in))
    if line = '' then iterate
    parse var line x ',' y ',' z
    key = x"_"y"_"z
    cubes.key = 1
    min_x = min(min_x, x); max_x = max(max_x, x)
    min_y = min(min_y, y); max_y = max(max_y, y)
    min_z = min(min_z, z); max_z = max(max_z, z)
  end
  call lineout file_in

  min_x = min_x - 1; min_y = min_y - 1; min_z = min_z - 1
  max_x = max_x + 1; max_y = max_y + 1; max_z = max_z + 1

  faces = 0
  q. = ''
  q_head = 1
  q_tail = 1

  start_pos = min_x min_y min_z
  start_key = min_x"_"min_y"_"min_z
  q.q_tail = start_pos
  q_tail = q_tail + 1
  seen.start_key = 1

  do while q_head < q_tail
    curr_pos = q.q_head
    q_head = q_head + 1
    parse var curr_pos curr_x curr_y curr_z

    do i = 1 to 6
      parse var d.i dx dy dz
      next_x = curr_x + dx
      next_y = curr_y + dy
      next_z = curr_z + dz

      if next_x < min_x | next_x > max_x then iterate
      if next_y < min_y | next_y > max_y then iterate
      if next_z < min_z | next_z > max_z then iterate

      next_key = next_x"_"next_y"_"next_z

      if cubes.next_key = 1 then
        faces = faces + 1
      else if seen.next_key = 0 then do
        seen.next_key = 1
        q.q_tail = next_x next_y next_z
        q_tail = q_tail + 1
      end
    end
  end

  say faces
return
