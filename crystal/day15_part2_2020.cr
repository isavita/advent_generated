def solve(turns)
  numbers = Hash(Int32, Int32).new
  starting_numbers = File.read_lines("input.txt")[0].split(',').map(&.to_i)
  last_spoken = 0
  turn = 1

  starting_numbers.each do |num|
    numbers[num] = turn
    last_spoken = num
    turn += 1
  end

  while turn <= turns
    if numbers.has_key?(last_spoken)
      age = turn - numbers[last_spoken] - 1
      numbers[last_spoken] = turn - 1
      last_spoken = age
    else
      numbers[last_spoken] = turn - 1
      last_spoken = 0
    end
    turn += 1
  end

  puts last_spoken
end

solve(2020) # Part 1
solve(30000000) # Part 2