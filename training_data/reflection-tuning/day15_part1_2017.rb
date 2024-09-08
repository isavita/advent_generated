# Read input
start_a, start_b = File.read('input.txt').split.map(&:to_i)

# Constants
FACTOR_A = 16807
FACTOR_B = 48271
DIVISOR = 2147483647
MASK = (1 << 16) - 1  # Mask for lowest 16 bits
PAIRS = 40_000_000

# Initialize
a, b = start_a, start_b
count = 0

# Main loop
PAIRS.times do
  # Generate next values
  a = (a * FACTOR_A) % DIVISOR
  b = (b * FACTOR_B) % DIVISOR

  # Compare lowest 16 bits
  count += 1 if (a & MASK) == (b & MASK)
end

# Print result
puts count
