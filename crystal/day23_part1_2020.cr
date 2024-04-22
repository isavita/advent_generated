File.open("input.txt", "r") do |file|
  input = file.gets_to_end.chomp

  cups = Array.new(input.size + 1, 0)
  current_cup = 0
  input.each_char_with_index do |char, i|
    cup = char.to_i
    if i == 0
      current_cup = cup
    end
    if i < input.size - 1
      next_cup = input[i + 1].to_i
      cups[cup] = next_cup
    end
  end
  first_cup = input[0].to_i
  last_cup = input[-1].to_i
  cups[last_cup] = first_cup

  100.times do
    pickup1 = cups[current_cup]
    pickup2 = cups[pickup1]
    pickup3 = cups[pickup2]

    cups[current_cup] = cups[pickup3]

    destination_cup = current_cup - 1
    destination_cup = input.size if destination_cup < 1
    while [pickup1, pickup2, pickup3].includes?(destination_cup)
      destination_cup -= 1
      destination_cup = input.size if destination_cup < 1
    end

    cups[pickup3] = cups[destination_cup]
    cups[destination_cup] = pickup1

    current_cup = cups[current_cup]
  end

  cup = cups[1]
  loop do
    print cup
    cup = cups[cup]
    break if cup == 1
  end
  puts
end