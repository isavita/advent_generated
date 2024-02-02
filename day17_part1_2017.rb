
steps = File.read('input.txt').to_i
buffer = [0]
current_position = 0

2017.times do |i|
  current_position = (current_position + steps) % buffer.length + 1
  buffer.insert(current_position, i + 1)
end

puts buffer[(current_position + 1) % buffer.length]
