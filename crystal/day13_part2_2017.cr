# Read input from file
input = File.read_lines("input.txt")

# Parse input
layers = input.map do |line|
  depth, range = line.split(": ").map(&.to_i)
  {depth: depth, range: range}
end

# Part 1: Find the severity of the whole trip
severity = 0
layers.each do |layer|
  if (layer[:depth] % ((layer[:range] - 1) * 2)) == 0
    severity += layer[:depth] * layer[:range]
  end
end
puts "Part 1: #{severity}"

# Part 2: Find the fewest number of picoseconds to delay the packet
delay = 0
loop do
  caught = false
  layers.each do |layer|
    if ((layer[:depth] + delay) % ((layer[:range] - 1) * 2)) == 0
      caught = true
      break
    end
  end
  break unless caught
  delay += 1
end
puts "Part 2: #{delay}"