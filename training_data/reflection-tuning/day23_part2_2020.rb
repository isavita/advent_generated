def simulate_crab_cups(input, num_cups, num_moves)
  # Initialize cups array
  cups = Array.new(num_cups + 1, 0)
  first_cup = input[0].to_i
  input.each_char.with_index do |cup, i|
    next_cup = i == input.length - 1 ? first_cup : input[i + 1].to_i
    cups[cup.to_i] = next_cup
  end

  if num_cups > input.length
    last_input_cup = input[-1].to_i
    cups[last_input_cup] = input.length + 1
    (input.length + 1..num_cups).each { |i| cups[i] = i + 1 }
    cups[num_cups] = first_cup
  end

  current = first_cup

  num_moves.times do
    # Pick up three cups
    pickup1 = cups[current]
    pickup2 = cups[pickup1]
    pickup3 = cups[pickup2]
    next_current = cups[pickup3]

    # Find destination
    destination = current - 1
    destination = num_cups if destination == 0
    while [pickup1, pickup2, pickup3].include?(destination)
      destination -= 1
      destination = num_cups if destination == 0
    end

    # Place picked up cups after destination
    cups[current] = next_current
    cups[pickup3] = cups[destination]
    cups[destination] = pickup1

    current = next_current
  end

  cups
end

# Read input from file
input = File.read('input.txt').strip

# Part 1
cups = simulate_crab_cups(input, input.length, 100)
result = []
current = cups[1]
8.times do
  result << current
  current = cups[current]
end
puts "Part 1: #{result.join}"

# Part 2
cups = simulate_crab_cups(input, 1_000_000, 10_000_000)
cup1 = cups[1]
cup2 = cups[cup1]
puts "Part 2: #{cup1 * cup2}"
