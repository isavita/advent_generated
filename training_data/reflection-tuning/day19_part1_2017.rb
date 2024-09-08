def follow_path(grid)
  directions = {up: [-1, 0], down: [1, 0], left: [0, -1], right: [0, 1]}
  y, x = 0, grid[0].index('|')
  dy, dx = directions[:down]
  letters = ""

  loop do
    y, x = y + dy, x + dx
    char = grid[y][x]

    case char
    when '|', '-'
      next
    when '+'
      dy, dx = directions.values.find { |new_dy, new_dx|
        new_y, new_x = y + new_dy, x + new_dx
        new_y.between?(0, grid.size - 1) &&
        new_x.between?(0, grid[0].size - 1) &&
        grid[new_y][new_x] != ' ' &&
        [new_dy, new_dx] != [-dy, -dx]
      }
    when /[A-Z]/
      letters += char
    when ' '
      break
    end
  end

  letters
end

grid = File.readlines('input.txt', chomp: true).map(&:chars)
puts follow_path(grid)
