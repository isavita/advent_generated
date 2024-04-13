serial = File.read('input.txt').strip.to_i

grid_size = 300
grid = Array.new(grid_size) { Array.new(grid_size, 0) }

(0...grid_size).each do |y|
  (0...grid_size).each do |x|
    rack_id = x + 11
    power_level = rack_id * (y + 1)
    power_level += serial
    power_level *= rack_id
    power_level = (power_level / 100) % 10
    power_level -= 5
    grid[y][x] = power_level
  end
end

max_power = -Float::INFINITY
max_x = max_y = max_size = 0

(1..grid_size).each do |size|
  break if size > 20  # Limit the maximum size to 20

  (0..grid_size - size).each do |y|
    (0..grid_size - size).each do |x|
      total_power = (0...size).sum { |dy| (0...size).sum { |dx| grid[y + dy][x + dx] } }
      if total_power > max_power
        max_power = total_power
        max_x, max_y, max_size = x + 1, y + 1, size
      end
    end
  end
end

puts "#{max_x},#{max_y},#{max_size}"