
File.open("input.txt") do |f|
  grid = [] of String
  moves = ""
  reading_map = true
  f.each_line do |line|
    if reading_map
      if line.includes?("#")
        grid << line
      else
        reading_map = false
        moves += line.chomp
      end
    else
      moves += line.chomp
    end
  end

  runes = grid.map { |row| row.chars }

  robot_r = 0
  robot_c = 0
  runes.each_with_index do |row, r|
    row.each_with_index do |cell, c|
      if cell == '@'
        robot_r, robot_c = r, c
      end
    end
  end

  dirs = {
    '^' => {-1, 0},
    'v' => {1, 0},
    '<' => {0, -1},
    '>' => {0, 1},
  }

  moves.each_char do |move|
    d = dirs[move]
    nr, nc = robot_r + d[0], robot_c + d[1]
    if runes[nr][nc] == '#'
      next
    elsif runes[nr][nc] == 'O'
      unless push_boxes(runes, nr, nc, d[0], d[1])
        next
      end
    end
    if runes[nr][nc] == '.' || runes[nr][nc] == 'O'
      runes[robot_r][robot_c], runes[nr][nc] = '.', '@'
      robot_r, robot_c = nr, nc
    end
  end

  sum = 0
  runes.each_with_index do |row, r|
    row.each_with_index do |cell, c|
      if cell == 'O'
        sum += r * 100 + c
      end
    end
  end

  puts sum
end

def push_boxes(runes, r, c, dr, dc)
  nr, nc = r + dr, c + dc
  if runes[nr][nc] == '#'
    return false
  end
  if runes[nr][nc] == 'O'
    unless push_boxes(runes, nr, nc, dr, dc)
      return false
    end
  end
  if runes[nr][nc] == '.'
    runes[nr][nc] = 'O'
    runes[r][c] = '.'
    return true
  end
  return false
end
