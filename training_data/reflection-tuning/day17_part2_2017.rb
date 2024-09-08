# Read input from file
steps = File.read('input.txt').to_i

# Part One
buffer = [0]
position = 0

(1..2017).each do |i|
  position = (position + steps) % buffer.size
  buffer.insert(position + 1, i)
  position += 1
end

puts "Part One: #{buffer[(buffer.index(2017) + 1) % buffer.size]}"

# Part Two
position = 0
value_after_zero = 0

(1..50_000_000).each do |i|
  position = (position + steps) % i
  value_after_zero = i if position == 0
  position += 1
end

puts "Part Two: #{value_after_zero}"
