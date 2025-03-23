
Open = '.'
Trees = '|'
Lumberyard = '#'

def read_input(filename : String) : Array(Array(Char))
  File.read_lines(filename).map { |line| line.strip.chars }
end

def transform(grid : Array(Array(Char))) : Array(Array(Char))
  new_grid = Array.new(grid.size) { Array.new(grid[0].size, ' ') }
  grid.each_with_index do |row, i|
    row.each_with_index do |acre, j|
      new_grid[i][j] = next_acre_state(grid, i, j)
    end
  end
  new_grid
end

def next_acre_state(grid : Array(Array(Char)), i : Int32, j : Int32) : Char
  acre_type = grid[i][j]
  case acre_type
  when Open
    if count_adjacent(grid, i, j, Trees) >= 3
      Trees
    else
      Open
    end
  when Trees
    if count_adjacent(grid, i, j, Lumberyard) >= 3
      Lumberyard
    else
      Trees
    end
  when Lumberyard
    if count_adjacent(grid, i, j, Lumberyard) >= 1 && count_adjacent(grid, i, j, Trees) >= 1
      Lumberyard
    else
      Open
    end
  else
    acre_type
  end
end

def count_adjacent(grid : Array(Array(Char)), i : Int32, j : Int32, acre_type : Char) : Int32
  count = 0
  (-1..1).each do |x|
    (-1..1).each do |y|
      next if x == 0 && y == 0
      new_i = i + x
      new_j = j + y
      if new_i >= 0 && new_i < grid.size && new_j >= 0 && new_j < grid[0].size && grid[new_i][new_j] == acre_type
        count += 1
      end
    end
  end
  count
end

def count_resources(grid : Array(Array(Char))) : Tuple(Int32, Int32)
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
  {wooded, lumberyards}
end

def grid_to_string(grid : Array(Array(Char))) : String
  grid.map { |row| row.join }.join("\n")
end

grid = read_input("input.txt")
seen_states = Hash(String, Int32).new
cycle_start = 0
cycle_length = 0

1000000000.times do |minute|
  state = grid_to_string(grid)
  if seen_states.has_key?(state)
    cycle_start = seen_states[state]
    cycle_length = minute - seen_states[state]
    break
  end
  seen_states[state] = minute
  grid = transform(grid)
end

remaining_minutes = (1000000000 - cycle_start) % cycle_length
remaining_minutes.times do
  grid = transform(grid)
end

wooded, lumberyards = count_resources(grid)
puts wooded * lumberyards
