
def calculate_new_fuel(current_position, new_position)
  diff = (current_position - new_position).abs
  (diff * (diff + 1)) / 2
end

positions = File.read("input.txt").split(",").map(&.to_i).sort

min_fuel = Int32::MAX
(positions.first..positions.last).each do |i|
  fuel = positions.sum { |pos| calculate_new_fuel(pos, i) }
  min_fuel = fuel if fuel < min_fuel
end

puts min_fuel
