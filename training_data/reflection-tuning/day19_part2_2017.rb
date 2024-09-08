def traverse_network(grid)
  directions = { 'up' => [-1, 0], 'down' => [1, 0], 'left' => [0, -1], 'right' => [0, 1] }
  row, col = 0, grid[0].index('|')
  dir = 'down'
  letters = ''
  steps = 0

  loop do
    steps += 1
    row += directions[dir][0]
    col += directions[dir][1]
    
    char = grid[row][col]
    case char
    when '|', '-'
      next
    when '+'
      dir = directions.keys.find { |d| d != dir && d != opposite(dir) && valid_move?(grid, row, col, directions[d]) }
    when /[A-Z]/
      letters += char
    when ' '
      break
    end
  end

  [letters, steps]
end

def opposite(dir)
  { 'up' => 'down', 'down' => 'up', 'left' => 'right', 'right' => 'left' }[dir]
end

def valid_move?(grid, row, col, delta)
  new_row, new_col = row + delta[0], col + delta[1]
  new_row.between?(0, grid.size - 1) && new_col.between?(0, grid[0].size - 1) && grid[new_row][new_col] != ' '
end

grid = File.readlines('input.txt', chomp: true).map(&:chars)
letters, steps = traverse_network(grid)

puts "Part 1: #{letters}"
puts "Part 2: #{steps}"
