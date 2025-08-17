require "set"

def loops(grid, start_x, start_y, start_dir)
  h = grid.size
  w = grid[0].size
  dirs = [{0, -1}, {1, 0}, {0, 1}, {-1, 0}]
  x = start_x
  y = start_y
  dir_idx = start_dir
  seen = Set(Tuple(Int32, Int32, Int32)).new
  2_000_000.times do
    state = {x, y, dir_idx}
    return true if seen.includes?(state)
    seen.add(state)
    dir_x, dir_y = dirs[dir_idx]
    nx = x + dir_x
    ny = y + dir_y
    return false if nx < 0 || nx >= w || ny < 0 || ny >= h
    if grid[ny][nx] == '#'
      dir_idx = (dir_idx + 1) % 4
      next
    end
    x = nx
    y = ny
  end
  false
end

input = File.read("input.txt")
lines = input.split('\n').map(&.strip).reject(&.empty?)
grid = lines.map { |l| l.chars }
h = grid.size
w = grid[0].size
start_x = start_y = 0
start_dir = 0
h.times do |i|
  w.times do |j|
    case grid[i][j]
    when '^'
      start_x = j
      start_y = i
      start_dir = 0
    when '>'
      start_x = j
      start_y = i
      start_dir = 1
    when 'v'
      start_x = j
      start_y = i
      start_dir = 2
    when '<'
      start_x = j
      start_y = i
      start_dir = 3
    end
  end
end
grid[start_y][start_x] = '.'
can_loop = 0
h.times do |y|
  w.times do |x|
    next if x == start_x && y == start_y
    next unless grid[y][x] == '.'
    grid[y][x] = '#'
    can_loop += 1 if loops(grid, start_x, start_y, start_dir)
    grid[y][x] = '.'
  end
end
puts can_loop

