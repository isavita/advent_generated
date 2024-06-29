
GRID_SIZE = 1000

grid = Array.new(GRID_SIZE) { [0] * GRID_SIZE }

File.foreach('input.txt') do |line|
  parts = line.split
  start_x, start_y = parts[-3].split(',').map(&:to_i)
  end_x, end_y = parts[-1].split(',').map(&:to_i)

  case parts[0]
  when 'turn'
    value = parts[1] == 'on' ? 1 : -1
    (start_x..end_x).each do |x|
      (start_y..end_y).each do |y|
        grid[x][y] = [grid[x][y] + value, 0].max
      end
    end
  when 'toggle'
    (start_x..end_x).each do |x|
      (start_y..end_y).each do |y|
        grid[x][y] += 2
      end
    end
  end
end

puts grid.sum { |row| row.sum }
