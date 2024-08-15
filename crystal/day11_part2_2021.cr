require "file_utils"

def read_input(filename)
  grid = [] of Array(Int32)
  File.each_line(filename) do |line|
    row = line.chomp.chars.map { |char| char.to_i }
    grid << row
  end
  grid
end

def simulate_step(grid)
  flashes = 0
  flashed = Set(Tuple(Int32, Int32)).new

  grid.each do |row|
    row.map! { |x| x + 1 }
  end

  grid.each_with_index do |row, y|
    row.each_index do |x|
      if grid[y][x] > 9
        flashes += flash(grid, x, y, flashed)
      end
    end
  end

  flashed.each do |coords|
    grid[coords[1]][coords[0]] = 0
  end

  flashes
end

def flash(grid, x, y, flashed)
  return 0 if flashed.includes?({x, y})

  flashed.add({x, y})
  flashes = 1
  directions = [
    {-1, -1},
    {-1, 0},
    {-1, 1},
    {0, -1},
    {0, 1},
    {1, -1},
    {1, 0},
    {1, 1},
  ]

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
step = 0
loop do
  step += 1
  flashes = simulate_step(grid)
  break if flashes == 100
end

puts step