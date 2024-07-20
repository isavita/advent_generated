
def read_input(file)
  File.readlines(file).map(&:chomp)
end

def print_grid(grid)
  grid.each { |row| puts row }
end

def move_sea_cucumbers(grid)
  height = grid.size
  width = grid[0].size
  moved = false

  # Move east-facing sea cucumbers
  new_grid = grid.map(&:dup)
  (0...height).each do |y|
    (0...width).each do |x|
      if grid[y][x] == '>'
        next_x = (x + 1) % width
        if grid[y][next_x] == '.'
          new_grid[y][x] = '.'
          new_grid[y][next_x] = '>'
          moved = true
        end
      end
    end
  end

  grid = new_grid.map(&:dup)

  # Move south-facing sea cucumbers
  new_grid = grid.map(&:dup)
  (0...height).each do |y|
    (0...width).each do |x|
      if grid[y][x] == 'v'
        next_y = (y + 1) % height
        if grid[next_y][x] == '.'
          new_grid[y][x] = '.'
          new_grid[next_y][x] = 'v'
          moved = true
        end
      end
    end
  end

  [new_grid, moved]
end

def simulate_sea_cucumbers(grid)
  steps = 0
  loop do
    steps += 1
    grid, moved = move_sea_cucumbers(grid)
    break unless moved
  end
  steps
end

def main
  grid = read_input('input.txt')
  steps = simulate_sea_cucumbers(grid)
  puts "The first step on which no sea cucumbers move is: #{steps}"
end

main
