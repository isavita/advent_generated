GRID_SIZE = 100
STEPS = 100

def count_on_neighbors(grid, x, y)
  on = 0
  (-1..1).each do |dx|
    (-1..1).each do |dy|
      next if dx.zero? && dy.zero?
      nx, ny = x + dx, y + dy
      on += 1 if nx >= 0 && nx < GRID_SIZE && ny >= 0 && ny < GRID_SIZE && grid[nx][ny]
    end
  end
  on
end

def step(grid)
  new_grid = Array.new(GRID_SIZE) { Array.new(GRID_SIZE, false) }
  (0...GRID_SIZE).each do |x|
    (0...GRID_SIZE).each do |y|
      on_neighbors = count_on_neighbors(grid, x, y)
      new_grid[x][y] = if grid[x][y]
                         on_neighbors == 2 || on_neighbors == 3
                       else
                         on_neighbors == 3
                       end
    end
  end
  new_grid[0][0] = new_grid[0][GRID_SIZE - 1] = new_grid[GRID_SIZE - 1][0] = new_grid[GRID_SIZE - 1][GRID_SIZE - 1] = true
  new_grid
end

grid = Array.new(GRID_SIZE) { Array.new(GRID_SIZE, false) }
File.readlines('input.txt').each_with_index do |line, y|
  line.chomp.each_char.with_index { |c, x| grid[x][y] = c == '#' }
end

grid[0][0] = grid[0][GRID_SIZE - 1] = grid[GRID_SIZE - 1][0] = grid[GRID_SIZE - 1][GRID_SIZE - 1] = true

STEPS.times { grid = step(grid) }

on_count = 0
grid.each do |row|
  row.each { |light| on_count += 1 if light }
end

puts on_count