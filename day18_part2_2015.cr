
grid_size = 100
steps = 100

def count_on_neighbors(grid, x, y, grid_size)
  on = 0
  (-1..1).each do |dx|
    (-1..1).each do |dy|
      next if dx == 0 && dy == 0
      nx, ny = x + dx, y + dy
      on += 1 if nx >= 0 && nx < grid_size && ny >= 0 && ny < grid_size && grid[nx][ny]
    end
  end
  on
end

def step(grid, grid_size)
  new_grid = Array.new(grid_size) { Array.new(grid_size, false) }

  (0...grid_size).each do |x|
    (0...grid_size).each do |y|
      on_neighbors = count_on_neighbors(grid, x, y, grid_size)
      new_grid[x][y] = if grid[x][y]
                         on_neighbors == 2 || on_neighbors == 3
                       else
                         on_neighbors == 3
                       end
    end
  end

  new_grid[0][0] = true
  new_grid[0][grid_size - 1] = true
  new_grid[grid_size - 1][0] = true
  new_grid[grid_size - 1][grid_size - 1] = true

  new_grid
end

file = File.open("input.txt")
grid = Array.new(grid_size) { Array.new(grid_size, false) }

file.each_line.with_index do |line, y|
  line.each_char.with_index do |c, x|
    grid[x][y] = c == '#'
  end
end

grid[0][0] = true
grid[0][grid_size - 1] = true
grid[grid_size - 1][0] = true
grid[grid_size - 1][grid_size - 1] = true

steps.times do
  grid = step(grid, grid_size)
end

on_count = grid.flatten.count(true)
puts on_count
