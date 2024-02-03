
steps = File.read('input.txt').to_i

buffer = [0]
current_position = 0

2017.times do |i|
  current_position = (current_position + steps) % buffer.length + 1
  buffer.insert(current_position, i + 1)
end

puts buffer[(current_position + 1) % buffer.length]

# Part Two
current_position = 0
value_after_zero = 0

1.upto(50000000) do |i|
  current_position = (current_position + steps) % i + 1
  value_after_zero = i if current_position == 1
end

puts value_after_zero
