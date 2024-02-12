
data = File.read("input.txt").chomp.to_i

grid_size = 300
grid = Array(Array(Int32)).new(grid_size) { Array(Int32).new(grid_size, 0) }

(0...grid_size).each do |y|
  (0...grid_size).each do |x|
    rack_id = x + 11
    power_level = rack_id * (y + 1)
    power_level += data
    power_level *= rack_id
    power_level = (power_level / 100) % 10
    power_level -= 5
    grid[y][x] = power_level.to_i
  end
end

max_power = -1 << 31
max_x = 0
max_y = 0

(0...grid_size-2).each do |y|
  (0...grid_size-2).each do |x|
    total_power = 0
    (0...3).each do |dy|
      (0...3).each do |dx|
        total_power += grid[y+dy][x+dx]
      end
    end
    if total_power > max_power
      max_power = total_power
      max_x = x + 1
      max_y = y + 1
    end
  end
end

puts "#{max_x},#{max_y}"
