
/* Rexx */
main:
  call read_input
  call process_moves
  call calculate_score
  exit

read_input:
  file = 'input.txt'
  rows = 0
  cols = 0
  moves = ''
  reading_map = 1
  grid. = ''

  do while lines(file) > 0
    line = strip(linein(file))
    if line = '' then do
      if rows > 0 then reading_map = 0
      iterate
    end

    if reading_map then do
      rows = rows + 1
      grid.rows = line
      cols = max(cols, length(line))
      robot_pos = pos('@', line)
      if robot_pos > 0 then do
        robot_r = rows
        robot_c = robot_pos
      end
    end
    else do
      moves = moves || line
    end
  end

  do r = 1 to rows
    grid.r = left(grid.r, cols, ' ')
  end
  return

process_moves:
  do i = 1 to length(moves)
    move = substr(moves, i, 1)
    dr = 0; dc = 0
    select
      when move = '^' then dr = -1
      when move = 'v' then dr = 1
      when move = '<' then dc = -1
      when move = '>' then dc = 1
      otherwise iterate
    end

    nr = robot_r + dr
    nc = robot_c + dc

    if nr < 1 | nr > rows | nc < 1 | nc > cols then iterate
    next_cell = substr(grid.nr, nc, 1)

    if next_cell = '#' then iterate
    if next_cell = 'O' then do
      if \push_boxes(nr, nc, dr, dc) then iterate
    end

    grid.robot_r = overlay('.', grid.robot_r, robot_c)
    grid.nr = overlay('@', grid.nr, nc)
    robot_r = nr
    robot_c = nc
  end
  return

push_boxes: procedure expose grid. rows cols
  arg r, c, dr, dc
  nr = r + dr
  nc = c + dc

  if nr < 1 | nr > rows | nc < 1 | nc > cols then return 0
  next_cell = substr(grid.nr, nc, 1)

  if next_cell = '#' then return 0
  if next_cell = 'O' then do
    if \push_boxes(nr, nc, dr, dc) then return 0
  end

  grid.nr = overlay('O', grid.nr, nc)
  grid.r = overlay('.', grid.r, c)
  return 1

calculate_score:
  total_sum = 0
  do r = 1 to rows
    do c = 1 to cols
      if substr(grid.r, c, 1) = 'O' then
        total_sum = total_sum + (r - 1) * 100 + (c - 1)
    end
  end
  say total_sum
  return
