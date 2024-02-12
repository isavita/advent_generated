
def calculate_fuel(current_position, new_position)
  (current_position - new_position).abs
end

positions = File.read("input.txt").split(",").map { |num| num.to_i }.sort

min_fuel = Int32::MAX
(positions.first..positions.last).each do |i|
  fuel = positions.sum { |pos| calculate_fuel(pos, i) }
  min_fuel = fuel if fuel < min_fuel
end

puts min_fuel
