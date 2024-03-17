GRID_SIZE = 1000

def process_instruction(instruction, grid)
  parts = instruction.split
  start_x, start_y = parts[-3].split(",").map(&.to_i)
  end_x, end_y = parts[-1].split(",").map(&.to_i)

  (start_x..end_x).each do |x|
    (start_y..end_y).each do |y|
      case parts[0]
      when "turn"
        grid[x][y] = parts[1] == "on"
      when "toggle"
        grid[x][y] = !grid[x][y]
      end
    end
  end
end

def count_lights(grid)
  grid.sum { |row| row.count(&.itself) }
end

grid = Array.new(GRID_SIZE) { Array.new(GRID_SIZE, false) }

File.each_line("input.txt") do |line|
  process_instruction(line, grid)
end

puts count_lights(grid)