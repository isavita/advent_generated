
instructions = File.readlines('input.txt').map(&:to_i)
current_index = 0
steps = 0

while current_index >= 0 && current_index < instructions.length
  next_index = current_index + instructions[current_index]
  instructions[current_index] += 1
  current_index = next_index
  steps += 1
end

puts steps
