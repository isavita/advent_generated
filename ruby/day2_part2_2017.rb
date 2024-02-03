
# Part 1
puts File.readlines('input.txt').map { |line| line.split.map(&:to_i).minmax.reduce(:-).abs }.sum

# Part 2
puts File.readlines('input.txt').map { |line| line.split.map(&:to_i).combination(2).map { |a, b| a % b == 0 ? a / b : b % a == 0 ? b / a : 0 }.sum }.sum
