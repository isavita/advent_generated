
file = File.open("input.txt")
grid = file.gets_to_end.split("\n").map { |line| line.chars }

x = 0
y = 0

grid[0].each_with_index do |cell, i|
  if cell == '|'
    x = i
    break
  end
end

dx = 0
dy = 1
steps = 0

loop do
  break if x < 0 || x >= grid[0].size || y < 0 || y >= grid.size

  cell = grid[y][x]

  break if cell == ' '

  steps += 1

  if cell == '+'
    if dx == 0
      dx, dy = (x > 0 && (grid[y][x-1] == '-' || (grid[y][x-1] >= 'A' && grid[y][x-1] <= 'Z'))) ? [-1, 0] : [1, 0]
    else
      dx, dy = (y > 0 && (grid[y-1][x] == '|' || (grid[y-1][x] >= 'A' && grid[y-1][x] <= 'Z'))) ? [0, -1] : [0, 1]
    end
  end

  x += dx
  y += dy
end

puts steps
