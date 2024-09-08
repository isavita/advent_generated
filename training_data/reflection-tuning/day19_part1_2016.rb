# Read the number of elves from the input file
n = File.read('input.txt').to_i

# Find the largest power of 2 less than or equal to n
largest_power_of_2 = 2 ** (Math.log2(n).floor)

# Calculate the left-over
left_over = n - largest_power_of_2

# Apply the formula to find the winning position
winning_position = 2 * left_over + 1

# Print the result
puts winning_position
