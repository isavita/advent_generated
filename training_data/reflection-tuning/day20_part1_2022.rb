# Read input
numbers = File.readlines('input.txt').map(&:to_i)

# Create list with original indices
list = numbers.each_with_index.map { |n, i| [n, i] }
size = list.size

# Mixing process
numbers.each_with_index do |num, orig_index|
  current_index = list.index { |n, i| i == orig_index }
  element = list.delete_at(current_index)
  new_index = (current_index + num) % (size - 1)
  list.insert(new_index, element)
end

# Find position of 0 and calculate grove coordinates
zero_index = list.index { |n, _| n == 0 }
grove_coords = [1000, 2000, 3000].map do |offset|
  list[(zero_index + offset) % size][0]
end

# Print the sum of grove coordinates
puts grove_coords.sum
