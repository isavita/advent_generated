input = File.readlines('input.txt').map(&:to_i)
index = 0
steps = 0

while index >= 0 && index < input.length
  offset = input[index]
  input[index] += offset >= 3 ? -1 : 1
  index += offset
  steps += 1
end

puts steps