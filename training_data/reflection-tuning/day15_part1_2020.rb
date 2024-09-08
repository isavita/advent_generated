# Read input from file
starting_numbers = File.read('input.txt').strip.split(',').map(&:to_i)

# Initialize variables
last_spoken = starting_numbers.last
turn_spoken = {}

# Initialize the hash with starting numbers
starting_numbers.each_with_index { |num, idx| turn_spoken[num] = idx + 1 }

# Iterate through turns
(starting_numbers.size + 1..2020).each do |turn|
  if turn_spoken.key?(last_spoken) && turn_spoken[last_spoken] != turn - 1
    next_number = turn - 1 - turn_spoken[last_spoken]
  else
    next_number = 0
  end

  turn_spoken[last_spoken] = turn - 1
  last_spoken = next_number
end

# Print the 2020th number spoken
puts last_spoken
