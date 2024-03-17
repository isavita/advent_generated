GRID_SIZE = 1000

def process_instruction(instruction, grid)
  parts = instruction.split
  start_x, start_y = parts[-3].split(",").map(&.to_i)
  end_x, end_y = parts[-1].split(",").map(&.to_i)

  (start_x..end_x).each do |x|
    (start_y..end_y).each do |y|
      case parts[0]
      when "turn"
        grid[x][y] += (parts[1] == "on") ? 1 : -1
        grid[x][y] = 0 if grid[x][y] < 0
      when "toggle"
        grid[x][y] += 2
      end
    end
  end
end

def total_brightness(grid)
  brightness = 0
  grid.each do |row|
    row.each { |light| brightness += light }
  end
  brightness
end

grid = Array.new(GRID_SIZE) { Array.new(GRID_SIZE, 0) }

File.each_line("input.txt") do |line|
  process_instruction(line, grid)
end

puts total_brightness(grid)