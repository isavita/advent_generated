
def read_input(filename)
  grid = []
  File.open(filename, "r") do |file|
    file.each_line do |line|
      row = line.chomp.chars.map(&:to_i)
      grid << row
    end
  end
  grid
end

def simulate_step(grid)
  flashes = 0
  flashed = {}

  grid.each_with_index do |row, y|
    row.each_with_index do |_, x|
      grid[y][x] += 1
    end
  end

  grid.each_with_index do |row, y|
    row.each_with_index do |_, x|
      if grid[y][x] > 9
        flashes += flash(grid, x, y, flashed)
      end
    end
  end

  flashed.each_key do |coords|
    grid[coords[1]][coords[0]] = 0
  end

  flashes
end

def flash(grid, x, y, flashed)
  return 0 if flashed[[x, y]]

  flashed[[x, y]] = true
  flashes = 1
  directions = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]]

  directions.each do |dir|
    new_x, new_y = x + dir[0], y + dir[1]
    if new_x >= 0 && new_x < grid[0].length && new_y >= 0 && new_y < grid.length
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
