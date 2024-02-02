
input = File.readlines("input.txt").map(&:chomp)

gamma_rate = ""
epsilon_rate = ""

(0..input[0].length - 1).each do |i|
  gamma_rate += input.map { |num| num[i] }.group_by(&:itself).values.max_by(&:size).first
  epsilon_rate += input.map { |num| num[i] }.group_by(&:itself).values.min_by(&:size).first
end

gamma_rate = gamma_rate.to_i(2)
epsilon_rate = epsilon_rate.to_i(2)

puts gamma_rate * epsilon_rate
