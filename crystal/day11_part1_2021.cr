require "file_utils"

def read_input(filename)
  grid = [] of Array(Int32)
  File.each_line(filename) do |line|
    row = line.chars.map { |char| char.to_i }
    grid << row
  end
  grid
end

def simulate_step(grid)
  flashes = 0
  flashed = {} of {Int32, Int32} => Bool

  grid.each_with_index do |row, y|
    row.each_with_index do |_, x|
      grid[y][x] += 1
    end
  end

  grid.each_with_index do |row, y|
    row.each_with_index do |energy, x|
      if energy > 9
        flashes += flash(grid, x, y, flashed)
      end
    end
  end

  flashed.each_key do |x, y|
    grid[y][x] = 0
  end

  flashes
end

def flash(grid, x, y, flashed)
  return 0 if flashed[{x, y}]?

  flashed[{x, y}] = true
  flashes = 1
  directions = [{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}]

  directions.each do |dir|
    new_x, new_y = x + dir[0], y + dir[1]
    if new_x >= 0 && new_x < grid[0].size && new_y >= 0 && new_y < grid.size
      grid[new_y][new_x] += 1
      if grid[new_y][new_x] > 9
        flashes += flash(grid, new_x, new_y, flashed)
      end
    end
  end

  flashes
end

grid = read_input("input.txt")
total_flashes = 0
100.times do
  total_flashes += simulate_step(grid)
end
puts total_flashes