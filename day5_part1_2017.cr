
data = File.read("input.txt").strip.split("\n").map(&.to_i)
index = 0
steps = 0

while index >= 0 && index < data.size
  jump = data[index]
  data[index] += 1
  index += jump
  steps += 1
end

puts steps
