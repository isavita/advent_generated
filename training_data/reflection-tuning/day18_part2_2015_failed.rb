def read_input(filename)
  File.readlines(filename).map(&:chomp).map(&:chars)
end

def count_neighbors(grid, x, y)
  count = 0
  (-1..1).each do |dx|
    (-1..1).each do |dy|
      next if dx == 0 && dy == 0
      nx, ny = (x + dx) % grid.length, (y + dy) % grid[0].length
      count += 1 if grid[nx][ny] == '#'
    end
  end
  count
end

def step(grid)
  new_grid = grid.map(&:dup)
  grid.each_with_index do |row, x|
    row.each_with_index do |light, y|
      neighbors = count_neighbors(grid, x, y)
      if light == '#'
        new_grid[x][y] = (neighbors == 2 || neighbors == 3) ? '#' : '.'
      else
        new_grid[x][y] = (neighbors == 3) ? '#' : '.'
      end
    end
  end
  new_grid
end

def part1(grid)
  100.times { grid = step(grid) }
  grid.flatten.count('#')
end

def part2(grid)
  grid[0][0] = grid[0][-1] = grid[-1][0] = grid[-1][-1] = '#'
  100.times do
    grid = step(grid)
    grid[0][0] = grid[0][-1] = grid[-1][0] = grid[-1][-1] = '#'
  end
  grid.flatten.count('#')
end

grid = read_input('input.txt')
puts "Part 1: #{part1(grid)}"
puts "Part 2: #{part2(grid)}"
