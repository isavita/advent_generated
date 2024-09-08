OPEN, TREES, LUMBERYARD = '.', '|', '#'

def parse_input(filename)
  File.readlines(filename).map(&:chomp).map(&:chars)
end

def count_adjacent(grid, x, y, type)
  count = 0
  (-1..1).each do |dx|
    (-1..1).each do |dy|
      next if dx == 0 && dy == 0
      nx, ny = x + dx, y + dy
      if nx.between?(0, grid.size - 1) && ny.between?(0, grid[0].size - 1)
        count += 1 if grid[nx][ny] == type
      end
    end
  end
  count
end

def simulate_minute(grid)
  new_grid = grid.map(&:dup)
  grid.each_with_index do |row, x|
    row.each_with_index do |acre, y|
      case acre
      when OPEN
        new_grid[x][y] = TREES if count_adjacent(grid, x, y, TREES) >= 3
      when TREES
        new_grid[x][y] = LUMBERYARD if count_adjacent(grid, x, y, LUMBERYARD) >= 3
      when LUMBERYARD
        new_grid[x][y] = OPEN unless count_adjacent(grid, x, y, LUMBERYARD) >= 1 && count_adjacent(grid, x, y, TREES) >= 1
      end
    end
  end
  new_grid
end

def count_resources(grid)
  trees = grid.flatten.count(TREES)
  lumberyards = grid.flatten.count(LUMBERYARD)
  [trees, lumberyards]
end

def solve(grid, minutes)
  seen = {}
  minute = 0
  while minute < minutes
    grid_string = grid.map(&:join).join
    if seen.key?(grid_string)
      cycle_length = minute - seen[grid_string]
      remaining = (minutes - minute) % cycle_length
      return solve(grid, remaining)
    end
    seen[grid_string] = minute
    grid = simulate_minute(grid)
    minute += 1
  end
  trees, lumberyards = count_resources(grid)
  trees * lumberyards
end

grid = parse_input('input.txt')
part1 = solve(grid, 10)
part2 = solve(grid, 1_000_000_000)

puts "Part 1: #{part1}"
puts "Part 2: #{part2}"
