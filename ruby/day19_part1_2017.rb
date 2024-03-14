File.open('input.txt', 'r') do |file|
  grid = file.readlines.map(&:chomp).map(&:chars)

  x, y = 0, 0
  grid[0].each_with_index { |cell, i| x = i if cell == '|' }

  dx, dy = 0, 1
  letters = []

  loop do
    break if x < 0 || x >= grid[0].length || y < 0 || y >= grid.length
    cell = grid[y][x]
    break if cell == ' '
    letters << cell if cell.match?(/[A-Z]/)

    if cell == '+'
      if dx.zero?
        dx, dy = x > 0 && (grid[y][x-1] == '-' || grid[y][x-1].match?(/[A-Z]/)) ? -1 : 1, 0
      else
        dx, dy = 0, y > 0 && (grid[y-1][x] == '|' || grid[y-1][x].match?(/[A-Z]/)) ? -1 : 1
      end
    end

    x += dx
    y += dy
  end

  puts letters.join
end