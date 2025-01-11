
serial = File.read("input.txt").strip.to_i
grid_size = 300
grid = Array.new(grid_size) { Array.new(grid_size, 0) }
summed_area = Array.new(grid_size + 1) { Array.new(grid_size + 1, 0) }

(0...grid_size).each do |y|
  (0...grid_size).each do |x|
    rack_id = x + 11
    power_level = rack_id * (y + 1)
    power_level += serial
    power_level *= rack_id
    power_level = (power_level // 100) % 10
    power_level -= 5
    grid[y][x] = power_level
    summed_area[y + 1][x + 1] = power_level + summed_area[y + 1][x] + summed_area[y][x + 1] - summed_area[y][x]
  end
end

max_power = Int32::MIN
max_x = 0
max_y = 0
max_size = 0

(1..grid_size).each do |size|
  (0..grid_size - size).each do |y|
    (0..grid_size - size).each do |x|
      total_power = summed_area[y + size][x + size] - summed_area[y][x + size] - summed_area[y + size][x] + summed_area[y][x]
      if total_power > max_power
        max_power = total_power
        max_x = x + 1
        max_y = y + 1
        max_size = size
      end
    end
  end
end

puts "#{max_x},#{max_y},#{max_size}"
