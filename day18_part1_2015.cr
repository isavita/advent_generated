
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
  new_grid = Array(Array(Bool)).new(grid_size) { Array(Bool).new(grid_size, false) }

  (0...grid_size).each do |x|
    (0...grid_size).each do |y|
      on_neighbors = count_on_neighbors(grid, x, y, grid_size)
      if grid[x][y]
        new_grid[x][y] = on_neighbors == 2 || on_neighbors == 3
      else
        new_grid[x][y] = on_neighbors == 3
      end
    end
  end

  new_grid
end

grid = Array(Array(Bool)).new(grid_size) { Array(Bool).new(grid_size, false) }

File.open("input.txt") do |file|
  y = 0
  file.each_line do |line|
    line.chars.each_with_index do |c, x|
      grid[x][y] = c == '#'
    end
    y += 1
  end
end

steps.times do
  grid = step(grid, grid_size)
end

on_count = grid.flatten.count { |light| light }

puts on_count
