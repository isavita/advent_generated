def next_state(grid)
  new_grid = Array.new(grid.size) { Array.new(grid[0].size) }
  grid.each_with_index do |row, x|
    row.each_with_index do |acre, y|
      trees = count_neighbors(grid, x, y, :|)
      lumberyards = count_neighbors(grid, x, y, :#)
      
      new_grid[x][y] = case acre
      when :.
        trees >= 3 ? :| : :.
      when :|
        lumberyards >= 3 ? :# : :|
      when :#
        (lumberyards >= 1 && trees >= 1) ? :# : :.
      end
    end
  end
  new_grid
end

def count_neighbors(grid, x, y, type)
  count = 0
  (-1..1).each do |dx|
    (-1..1).each do |dy|
      next if dx == 0 && dy == 0
      nx, ny = x + dx, y + dy
      if nx.between?(0, grid.size - 1) && ny.between?(0, grid[0].size - 1)
        count += 1 if grid[nx][ny] == type
      end
    end
  end
  count
end

def count_resources(grid)
  trees = 0
  lumberyards = 0
  grid.each do |row|
    trees += row.count(:|)
    lumberyards += row.count(:#)
  end
  [trees, lumberyards]
end

def simulate(grid, minutes)
  minutes.times do
    grid = next_state(grid)
  end
  grid
end

# Read input
grid = File.readlines('input.txt').map(&:chomp).map { |line| line.chars.map(&:to_sym) }

# Simulate for 10 minutes
final_grid = simulate(grid, 10)

# Count resources
trees, lumberyards = count_resources(final_grid)

# Calculate total resource value
total_resource_value = trees * lumberyards

puts "Total resource value after 10 minutes: #{total_resource_value}"
