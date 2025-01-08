
grid = File.read("input.txt").lines.map(&.chars)
h = grid.size
w = grid[0].size
x, y, dir_idx = 0, 0, 0
dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]]
found = false

h.times do |i|
  w.times do |j|
    case grid[i][j]
    when '^'
      x, y = j, i
      dir_idx = 0
      found = true
    when '>'
      x, y = j, i
      dir_idx = 1
      found = true
    when 'v'
      x, y = j, i
      dir_idx = 2
      found = true
    when '<'
      x, y = j, i
      dir_idx = 3
      found = true
    end
    break if found
  end
  break if found
end

visited = Set(Tuple(Int32, Int32)).new
visited << {x, y}

loop do
  dir_x, dir_y = dirs[dir_idx]
  nx, ny = x + dir_x, y + dir_y
  break if nx < 0 || nx >= w || ny < 0 || ny >= h
  if grid[ny][nx] == '#'
    dir_idx = (dir_idx + 1) % 4
    next
  end
  x, y = nx, ny
  visited << {x, y}
end

puts visited.size
