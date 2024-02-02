
coordinates = File.readlines('input.txt').map { |line| line.split(', ').map(&:to_i) }

max_x = coordinates.map { |c| c[0] }.max
max_y = coordinates.map { |c| c[1] }.max

grid = Array.new(max_x + 1) { Array.new(max_y + 1) }

max_distance = 10000
region_size = 0

(max_x + 1).times do |x|
  (max_y + 1).times do |y|
    total_distance = coordinates.sum { |c| (x - c[0]).abs + (y - c[1]).abs }
    region_size += 1 if total_distance < max_distance
  end
end

puts region_size
