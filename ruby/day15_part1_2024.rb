
grid = []
moves = ""
reading_map = true

File.readlines("input.txt").each do |line|
  line = line.strip
  if reading_map
    if line.include?("#")
      grid << line.chars
    else
      reading_map = false
      moves += line
    end
  else
    moves += line
  end
end

robot_r, robot_c = nil
grid.each_with_index do |row, r|
  row.each_with_index do |cell, c|
    if cell == '@'
      robot_r, robot_c = r, c
      break
    end
  end
  break if robot_r
end

dirs = {
  '^' => [-1, 0],
  'v' => [1, 0],
  '<' => [0, -1],
  '>' => [0, 1]
}

def push_boxes(grid, r, c, dr, dc)
  nr, nc = r + dr, c + dc
  return false if grid[nr][nc] == '#'
  if grid[nr][nc] == 'O'
    return false unless push_boxes(grid, nr, nc, dr, dc)
  end
  if grid[nr][nc] == '.'
    grid[nr][nc] = 'O'
    grid[r][c] = '.'
    return true
  end
  return false
end

moves.each_char do |move|
  d = dirs[move]
  nr, nc = robot_r + d[0], robot_c + d[1]
  next if grid[nr][nc] == '#'
  if grid[nr][nc] == 'O'
    next unless push_boxes(grid, nr, nc, d[0], d[1])
  end
  if grid[nr][nc] == '.' || grid[nr][nc] == 'O'
    grid[robot_r][robot_c], grid[nr][nc] = '.', '@'
    robot_r, robot_c = nr, nc
  end
end

sum = 0
grid.each_with_index do |row, r|
  row.each_with_index do |cell, c|
    if cell == 'O'
      sum += r * 100 + c
    end
  end
end

puts sum
