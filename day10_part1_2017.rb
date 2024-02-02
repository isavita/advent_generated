
input = File.read("input.txt").strip.split(",").map(&:to_i)
list = (0..255).to_a
current_position = 0
skip_size = 0

input.each do |length|
  list.rotate!(current_position)
  list[0, length] = list[0, length].reverse
  list.rotate!(-current_position)
  
  current_position = (current_position + length + skip_size) % list.length
  skip_size += 1
end

puts list[0] * list[1]
