
# Read input from file
input = File.read('input.txt')

# Parse input and extract node information
nodes = input.scan(/x(\d+)-y(\d+)\s+\d+T\s+(\d+)T\s+(\d+)T/).map { |x, y, used, avail|
  { x: x.to_i, y: y.to_i, used: used.to_i, avail: avail.to_i }
}

# Count viable pairs
viable_pairs = 0
nodes.combination(2) do |a, b|
  if a[:used] > 0 && a[:used] <= b[:avail]
    viable_pairs += 1
  end
  if b[:used] > 0 && b[:used] <= a[:avail]
    viable_pairs += 1
  end
end

# Return the result
puts viable_pairs
