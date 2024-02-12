
file = File.open("input.txt")
grid = file.gets_to_end.split("\n").map { |line| line.chars }

x, y = 0, 0
grid[0].each_with_index do |cell, i|
  if cell == '|'
    x = i
    break
  end
end

dx, dy = 0, 1
letters = [] of Char

loop do
  break if x < 0 || x >= grid[0].size || y < 0 || y >= grid.size

  cell = grid[y][x]

  break if cell == ' '

  letters << cell if cell >= 'A' && cell <= 'Z'

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

puts letters.join
