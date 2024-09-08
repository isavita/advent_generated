# Read and parse input
nodes = File.readlines('input.txt').drop(2).map do |line|
  x, y, size, used, avail = line.scan(/\d+/).map(&:to_i)
  { x: x, y: y, size: size, used: used, avail: avail }
end

# Count viable pairs
viable_pairs = 0
nodes.each_with_index do |a, i|
  nodes.each_with_index do |b, j|
    next if i == j || a[:used] == 0
    viable_pairs += 1 if a[:used] <= b[:avail]
  end
end

puts viable_pairs
