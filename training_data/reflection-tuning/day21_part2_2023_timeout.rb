require 'set'

def read_grid(filename)
  File.readlines(filename).map(&:chomp).map(&:chars)
end

def find_start(grid)
  grid.each_with_index do |row, y|
    row.each_with_index do |cell, x|
      return [y, x] if cell == 'S'
    end
  end
end

def bfs(grid, start, steps)
  height, width = grid.size, grid[0].size
  queue = [[start, steps]]
  visited = Set.new
  reachable = Set.new

  while !queue.empty?
    (y, x), remaining_steps = queue.shift
    next if visited.include?([y, x, remaining_steps])
    visited.add([y, x, remaining_steps])

    reachable.add([y % height, x % width]) if remaining_steps.even?

    next if remaining_steps == 0

    [[y-1, x], [y+1, x], [y, x-1], [y, x+1]].each do |ny, nx|
      if grid[ny % height][nx % width] != '#'
        queue << [[ny, nx], remaining_steps - 1]
      end
    end
  end

  reachable.size
end

grid = read_grid('input.txt')
start = find_start(grid)
size = grid.size

# Calculate key values
odd_plots = bfs(grid, start, 131)
even_plots = bfs(grid, start, 130)
corner_t = bfs(grid, [size-1, start[1]], 64)
corner_r = bfs(grid, [start[0], 0], 64)
corner_b = bfs(grid, [0, start[1]], 64)
corner_l = bfs(grid, [start[0], size-1], 64)
small_tr = bfs(grid, [size-1, 0], 130)
small_tl = bfs(grid, [size-1, size-1], 130)
small_br = bfs(grid, [0, 0], 130)
small_bl = bfs(grid, [0, size-1], 130)
big_tr = bfs(grid, [size-1, 0], 195)
big_tl = bfs(grid, [size-1, size-1], 195)
big_br = bfs(grid, [0, 0], 195)
big_bl = bfs(grid, [0, size-1], 195)

n = 202300

# Calculate the final result
result = (n+1)**2 * odd_plots + n**2 * even_plots
result += corner_t + corner_r + corner_b + corner_l
result += n * (small_tr + small_tl + small_br + small_bl)
result += (n+1) * (big_tr + big_tl + big_br + big_bl)

puts result
