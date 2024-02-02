numbers = File.readlines('input.txt').map(&:to_i)
result = numbers.combination(2).find { |a, b| a + b == 2020 }.reduce(:*)
puts result