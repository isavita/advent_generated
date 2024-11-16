
def parse_grid(input : String)
  input.lines.map(&.chars)
end

def step(grid : Array(Array(Char))) : {Array(Array(Char)), Bool}
  height = grid.size
  width = grid[0].size
  moved = false
  
  # East-facing sea cucumbers move first
  east_moves = [] of {Int32, Int32}
  grid.each_with_index do |row, y|
    row.each_with_index do |cucumber, x|
      if cucumber == '>' 
        next_x = (x + 1) % width
        if grid[y][next_x] == '.'
          east_moves << {y, x}
        end
      end
    end
  end

  east_moves.each do |move|
    y, x = move
    next_x = (x + 1) % width
    grid[y][x] = '.'
    grid[y][next_x] = '>'
    moved = true
  end

  # South-facing sea cucumbers move next
  south_moves = [] of {Int32, Int32}
  grid.each_with_index do |row, y|
    row.each_with_index do |cucumber, x|
      if cucumber == 'v'
        next_y = (y + 1) % height
        if grid[next_y][x] == '.'
          south_moves << {y, x}
        end
      end
    end
  end

  south_moves.each do |move|
    y, x = move
    next_y = (y + 1) % height
    grid[y][x] = '.'
    grid[next_y][x] = 'v'
    moved = true
  end

  {grid, moved}
end

def solve(grid : Array(Array(Char))) : Int32
  steps = 0
  moved = true

  while moved
    grid, moved = step(grid)
    steps += 1
  end

  steps
end

# Read input from file
input = File.read("input.txt")
grid = parse_grid(input)

# Solve and print result
puts solve(grid)
