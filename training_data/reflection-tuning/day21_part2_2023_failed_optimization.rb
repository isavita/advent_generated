def count_reachable_plots(input, steps)
  grid = input.split("\n").map(&:chars)
  height, width = grid.size, grid[0].size
  start = find_start(grid)

  grid_size = width  # Assuming square grid
  full_grid_distance = steps / grid_size
  remaining_steps = steps % grid_size

  even_grids = (full_grid_distance - 1) ** 2
  odd_grids = full_grid_distance ** 2

  corners = [
    bfs(grid, start, grid_size - 1),
    bfs(grid, start, grid_size * 3 - 1) - bfs(grid, start, grid_size * 2 - 1)
  ]

  edges = [
    bfs(grid, start, grid_size / 2 - 1),
    bfs(grid, start, grid_size * 3 / 2 - 1) - bfs(grid, start, grid_size / 2)
  ]

  result = odd_grids * corners[1] + even_grids * corners[0]
  result += (full_grid_distance - 1) * edges[1] * 4 + full_grid_distance * edges[0] * 4

  result
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
  queue = [[start[0], start[1], steps]]
  visited = Set.new
  count = 0

  while !queue.empty?
    y, x, remaining = queue.shift
    next if visited.include?([y, x, remaining % 2])
    visited.add([y, x, remaining % 2])

    count += 1 if remaining % 2 == 0

    next if remaining == 0

    [[0, 1], [0, -1], [1, 0], [-1, 0]].each do |dy, dx|
      ny, nx = y + dy, x + dx
      if ny >= 0 && ny < height && nx >= 0 && nx < width && grid[ny][nx] != '#'
        queue << [ny, nx, remaining - 1]
      end
    end
  end

  count
end

# Example usage:
input = File.read('input.txt')
puts count_reachable_plots(input, 26501365)
