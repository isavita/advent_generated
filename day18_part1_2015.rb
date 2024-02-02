
def count_neighbors(grid, x, y)
  count = 0
  (-1..1).each do |dx|
    (-1..1).each do |dy|
      next if dx == 0 && dy == 0
      nx = x + dx
      ny = y + dy
      count += 1 if nx >= 0 && nx < grid.length && ny >= 0 && ny < grid[0].length && grid[nx][ny] == '#'
    end
  end
  count
end

def next_state(grid, x, y)
  neighbors_on = count_neighbors(grid, x, y)
  if grid[x][y] == '#'
    neighbors_on == 2 || neighbors_on == 3 ? '#' : '.'
  else
    neighbors_on == 3 ? '#' : '.'
  end
end

def animate(grid)
  new_grid = Array.new(grid.length) { Array.new(grid[0].length, '.') }
  grid.each_with_index do |row, x|
    row.each_with_index do |cell, y|
      new_grid[x][y] = next_state(grid, x, y)
    end
  end
  new_grid
end

grid = File.readlines('input.txt').map(&:chomp).map(&:chars)

100.times do
  grid = animate(grid)
end

puts grid.flatten.count('#')
