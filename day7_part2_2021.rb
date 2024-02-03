
positions = File.read('input.txt').split(',').map(&:to_i)

# Part 1
def fuel_cost(positions, target)
  positions.sum { |pos| (pos - target).abs }
end

median = positions.sort[positions.length / 2]
part1_fuel = fuel_cost(positions, median)
puts part1_fuel

# Part 2
def fuel_cost_increasing(positions, target)
  positions.sum { |pos| d = (pos - target).abs; d * (d + 1) / 2 }
end

mean = positions.sum / positions.size.to_f
candidates = [mean.floor, mean.ceil]
part2_fuel = candidates.map { |candidate| fuel_cost_increasing(positions, candidate) }.min
puts part2_fuel
