
input = File.readlines("input.txt").map(&:strip)

def adjacent_acres(x, y, grid)
  adjacents = []

  (x-1..x+1).each do |i|
    (y-1..y+1).each do |j|
      next if i == x && j == y
      adjacents << grid[i][j] if i >= 0 && j >= 0 && i < grid.length && j < grid[0].length
    end
  end

  adjacents
end

def next_state(x, y, grid)
  current = grid[x][y]
  adjacents = adjacent_acres(x, y, grid)

  case current
  when "."
    adjacents.count("|") >= 3 ? "|" : "."
  when "|"
    adjacents.count("#") >= 3 ? "#" : "|"
  when "#"
    adjacents.count("#") >= 1 && adjacents.count("|") >= 1 ? "#" : "."
  end
end

def resource_value(grid)
  wooded = grid.flatten.count("|")
  lumberyards = grid.flatten.count("#")
  wooded * lumberyards
end

grid = input.map(&:chars)

10.times do
  new_grid = Array.new(grid.length) { Array.new(grid[0].length) }

  grid.each_with_index do |row, i|
    row.each_with_index do |cell, j|
      new_grid[i][j] = next_state(i, j, grid)
    end
  end

  grid = new_grid
end

puts resource_value(grid)
