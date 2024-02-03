
sum = File.read('input.txt').split(',').map { |step| step.chars.reduce(0) { |acc, char| (acc + char.ord) * 17 % 256 } }.sum
puts sum
