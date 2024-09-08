def count_neighbors(grid, x, y)
  count = 0
  (-1..1).each do |dx|
    (-1..1).each do |dy|
      next if dx == 0 && dy == 0
      nx, ny = x + dx, y + dy
      count += 1 if nx.between?(0, 99) && ny.between?(0, 99) && grid[ny][nx]
    end
  end
  count
end

def update_grid(current, next_grid)
  100.times do |y|
    100.times do |x|
      neighbors = count_neighbors(current, x, y)
      if current[y][x]
        next_grid[y][x] = (neighbors == 2 || neighbors == 3)
      else
        next_grid[y][x] = (neighbors == 3)
      end
    end
  end
end

# Read input
grid = File.readlines('input.txt', chomp: true).map { |line| line.chars.map { |c| c == '#' } }
next_grid = Array.new(100) { Array.new(100, false) }

# Simulate 100 steps
100.times do
  update_grid(grid, next_grid)
  grid, next_grid = next_grid, grid
end

# Count lights on
lights_on = grid.sum { |row| row.count(true) }

puts lights_on
