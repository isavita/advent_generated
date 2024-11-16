
def tilt_north(grid : Array(Array(Char)))
  width = grid[0].size
  height = grid.size

  # Iterate through each column
  (0...width).each do |col|
    # Track the next available row for a rock to roll to
    next_row = 0

    # Iterate through each row from top to bottom
    (0...height).each do |row|
      case grid[row][col]
      when 'O'
        # Move rock to next available row
        if next_row != row
          grid[next_row][col] = 'O'
          grid[row][col] = '.'
        end
        next_row += 1
      when '#'
        # Cube rock blocks further rolling, reset next_row
        next_row = row + 1
      end
    end
  end

  grid
end

def calculate_load(grid : Array(Array(Char)))
  height = grid.size
  total_load = 0

  grid.each_with_index do |row, row_idx|
    row.each do |cell|
      total_load += (height - row_idx) if cell == 'O'
    end
  end

  total_load
end

# Read input from file
grid = File.read_lines("input.txt").map(&.chars)

# Tilt platform north
tilted_grid = tilt_north(grid)

# Calculate total load
puts calculate_load(tilted_grid)
