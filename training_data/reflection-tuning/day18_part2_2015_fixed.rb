def count_neighbors(grid, x, y)
  count = 0
  (-1..1).each do |dx|
    (-1..1).each do |dy|
      next if dx == 0 && dy == 0
      nx, ny = x + dx, y + dy
      count += 1 if nx.between?(0, 99) && ny.between?(0, 99) && grid[ny][nx] == '#'
    end
  end
  count
end

def next_state(grid, part2 = false)
  new_grid = Array.new(100) { Array.new(100, '.') }
  
  100.times do |y|
    100.times do |x|
      neighbors = count_neighbors(grid, x, y)
      if grid[y][x] == '#'
        new_grid[y][x] = '#' if neighbors == 2 || neighbors == 3
      else
        new_grid[y][x] = '#' if neighbors == 3
      end
    end
  end
  
  if part2
    new_grid[0][0] = new_grid[0][99] = new_grid[99][0] = new_grid[99][99] = '#'
  end
  
  new_grid
end

def solve(input, steps, part2 = false)
  grid = input.split("\n").map(&:chars)
  
  if part2
    grid[0][0] = grid[0][99] = grid[99][0] = grid[99][99] = '#'
  end
  
  steps.times do
    grid = next_state(grid, part2)
  end
  
  grid.flatten.count('#')
end

input = File.read('input.txt')

puts "Part 1: #{solve(input, 100)}"
puts "Part 2: #{solve(input, 100, true)}"
