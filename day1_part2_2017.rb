
input = File.read('input.txt').chomp

# Part One
puts input.chars.each_with_index.reduce(0) { |sum, (digit, index)| digit == input[(index + 1) % input.length] ? sum + digit.to_i : sum }

# Part Two
puts input.chars.each_with_index.reduce(0) { |sum, (digit, index)| digit == input[(index + input.length / 2) % input.length] ? sum + digit.to_i : sum }
