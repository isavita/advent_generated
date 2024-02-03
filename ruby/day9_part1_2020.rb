
numbers = File.readlines('input.txt').map(&:to_i)

def valid_number?(number, preamble)
  preamble.combination(2).any? { |pair| pair.sum == number }
end

invalid_number = numbers[25..].find.with_index do |number, index|
  !valid_number?(number, numbers[index...index + 25])
end

puts invalid_number
