require 'set'

def dijkstra(grid)
  height, width = grid.size, grid[0].size
  distances = Array.new(height) { Array.new(width, Float::INFINITY) }
  distances[0][0] = 0
  queue = [[0, 0, 0]]  # [distance, row, col]
  visited = Set.new

  while !queue.empty?
    dist, row, col = queue.shift
    next if visited.include?([row, col])
    visited.add([row, col])

    return dist if row == height - 1 && col == width - 1

    [[0, 1], [1, 0], [0, -1], [-1, 0]].each do |dr, dc|
      new_row, new_col = row + dr, col + dc
      if new_row.between?(0, height - 1) && new_col.between?(0, width - 1)
        new_dist = dist + grid[new_row][new_col]
        if new_dist < distances[new_row][new_col]
          distances[new_row][new_col] = new_dist
          queue << [new_dist, new_row, new_col]
          queue.sort_by! { |d, _, _| d }
        end
      end
    end
  end
end

def expand_grid(grid)
  height, width = grid.size, grid[0].size
  new_grid = Array.new(height * 5) { Array.new(width * 5, 0) }

  5.times do |i|
    5.times do |j|
      height.times do |r|
        width.times do |c|
          value = (grid[r][c] + i + j - 1) % 9 + 1
          new_grid[i * height + r][j * width + c] = value
        end
      end
    end
  end

  new_grid
end

# Read input
grid = File.readlines('input.txt', chomp: true).map { |line| line.chars.map(&:to_i) }

# Part 1
puts "Part 1: #{dijkstra(grid)}"

# Part 2
expanded_grid = expand_grid(grid)
puts "Part 2: #{dijkstra(expanded_grid)}"
