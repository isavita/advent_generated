# Read input from file and convert to array of integers
input = File.read('input.txt').strip.chars.map(&:to_i)

# Function to generate pattern for a given position
def generate_pattern(position, length)
  base_pattern = [0, 1, 0, -1]
  pattern = base_pattern.flat_map { |n| [n] * position }
  pattern.cycle.drop(1).take(length).to_a
end

# Precompute patterns for the first 8 positions
patterns = (1..8).map { |i| generate_pattern(i, input.length) }

# Perform FFT for 100 phases
100.times do
  input = patterns.map do |pattern|
    input.zip(pattern).sum { |a, b| a * b }.abs % 10
  end
end

# Print the first 8 digits of the result
puts input.first(8).join
