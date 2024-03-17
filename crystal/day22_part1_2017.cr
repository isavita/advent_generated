File.open("input.txt") do |file|
  grid = Hash(Tuple(Int32, Int32), Bool).new
  start_x, start_y = 0, 0

  file.each_line.with_index do |line, y|
    line.each_char.with_index do |c, x|
      if c == '#'
        grid[{x, y}] = true
      end
    end
    start_x, start_y = line.size // 2, y // 2
  end

  dx = [0, 1, 0, -1]
  dy = [-1, 0, 1, 0]

  x, y, dir = start_x, start_y, 0
  infected_count = 0

  10000.times do
    pos = {x, y}
    if grid.has_key?(pos) && grid[pos]
      dir = (dir + 1) % 4
      grid.delete(pos)
    else
      dir = (dir - 1 + 4) % 4
      grid[pos] = true
      infected_count += 1
    end
    x += dx[dir]
    y += dy[dir]
  end

  puts infected_count
end