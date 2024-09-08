def find_winning_elf(num_elves)
  power_of_3 = 3 ** Math.log(num_elves, 3).floor
  diff = num_elves - power_of_3

  if diff == 0
    return num_elves
  elsif diff <= num_elves / 2
    return diff
  else
    return 2 * diff - num_elves
  end
end

# Read input from file
num_elves = File.read('input.txt').to_i

# Calculate and print the result
puts find_winning_elf(num_elves)
