def find_nth_number(starting_numbers, target_turn)
  last_spoken = {}
  last_number = nil

  # Initialize with starting numbers
  starting_numbers.each_with_index do |num, idx|
    last_spoken[num] = idx + 1
    last_number = num
  end

  turn = starting_numbers.size + 1

  while turn <= target_turn
    if last_spoken.key?(last_number) && last_spoken[last_number] < turn - 1
      next_number = turn - 1 - last_spoken[last_number]
    else
      next_number = 0
    end

    last_spoken[last_number] = turn - 1
    last_number = next_number
    turn += 1
  end

  last_number
end

# Read input from file
starting_numbers = File.read('input.txt').strip.split(',').map(&:to_i)

# Solve Part 1
part1_result = find_nth_number(starting_numbers, 2020)
puts "Part 1: The 2020th number spoken is #{part1_result}"

# Solve Part 2
part2_result = find_nth_number(starting_numbers, 30_000_000)
puts "Part 2: The 30000000th number spoken is #{part2_result}"
