
data = File.read("input.txt").chomp.split("\n").map { |line| line.to_i }

index = 0
steps = 0

while index >= 0 && index < data.size
  jump = data[index]

  if jump >= 3
    data[index] -= 1
  else
    data[index] += 1
  end

  index += jump
  steps += 1
end

puts steps
