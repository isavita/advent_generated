
Open = '.'
Trees = '|'
Lumberyard = '#'
Size = 50

def read_input(filename)
  grid = []
  File.open(filename, "r") do |file|
    file.each_line do |line|
      row = line.chomp.chars
      grid << row
    end
  end
  grid
end

def transform(grid)
  new_grid = Array.new(grid.length) { Array.new(grid[0].length, nil) }
  grid.each_with_index do |row, i|
    row.each_with_index do |_, j|
      new_grid[i][j] = next_acre_state(grid, i, j)
    end
  end
  new_grid
end

def next_acre_state(grid, i, j)
  case grid[i][j]
  when Open
    return Trees if count_adjacent(grid, i, j, Trees) >= 3
  when Trees
    return Lumberyard if count_adjacent(grid, i, j, Lumberyard) >= 3
  when Lumberyard
    return Lumberyard if count_adjacent(grid, i, j, Lumberyard) >= 1 && count_adjacent(grid, i, j, Trees) >= 1
    return Open
  end
  grid[i][j]
end

def count_adjacent(grid, i, j, acre_type)
  count = 0
  (-1..1).each do |x|
    (-1..1).each do |y|
      next if x == 0 && y == 0
      if i + x >= 0 && i + x < grid.length && j + y >= 0 && j + y < grid[i].length && grid[i + x][j + y] == acre_type
        count += 1
      end
    end
  end
  count
end

def count_resources(grid)
  wooded = 0
  lumberyards = 0
  grid.each do |row|
    row.each do |acre|
      case acre
      when Trees
        wooded += 1
      when Lumberyard
        lumberyards += 1
      end
    end
  end
  [wooded, lumberyards]
end

grid = read_input("input.txt")
seen_states = {}
cycle_start = 0
cycle_length = 0
minute = 0

loop do
  state = grid.map(&:join).join("\n")
  if seen_states[state]
    cycle_start = seen_states[state]
    cycle_length = minute - seen_states[state]
    break
  end
  seen_states[state] = minute
  grid = transform(grid)
  minute += 1
end

remaining_minutes = (1000000000 - cycle_start) % cycle_length
remaining_minutes.times { grid = transform(grid) }

wooded, lumberyards = count_resources(grid)
puts wooded * lumberyards
