
input = File.read('input.txt').split(',').map(&:to_i)
median = input.sort[input.size / 2]
puts input.map { |pos| (pos - median).abs }.sum
