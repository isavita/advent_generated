File.open("input.txt", "r") do |file|
  grid = file.readlines.map(&:chomp).map(&:chars)

  x, y = 0, 0
  grid[0].each_with_index { |cell, i| x = i if cell == "|" }

  dx, dy = 0, 1
  steps = 0

  while x >= 0 && x < grid[0].length && y >= 0 && y < grid.length
    cell = grid[y][x]
    break if cell == " "

    steps += 1

    if cell == "+"
      if dx == 0
        dx, dy = x > 0 && ["-", *"A".."Z"].include?(grid[y][x-1]) ? -1 : 1, 0
      else
        dx, dy = 0, y > 0 && ["|", *"A".."Z"].include?(grid[y-1][x]) ? -1 : 1
      end
    end

    x += dx
    y += dy
  end

  puts steps
end