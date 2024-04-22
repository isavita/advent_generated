file = File.open("input.txt")
input = file.gets_to_end.chomp

cups = Array(Int32).new(1000001, 0)
last_cup = 0

input.each_char_with_index do |char, i|
  cup = char.to_i
  cups[last_cup] = cup
  last_cup = cup
end

(10..1000000).each do |i|
  cups[last_cup] = i
  last_cup = i
end
cups[last_cup] = input[0].to_i

current_cup = input[0].to_i
total_moves = 10_000_000

total_moves.times do |i|
  pickup1 = cups[current_cup]
  pickup2 = cups[pickup1]
  pickup3 = cups[pickup2]

  cups[current_cup] = cups[pickup3]

  destination_cup = current_cup - 1
  destination_cup = 1000000 if destination_cup == 0
  while [pickup1, pickup2, pickup3].includes?(destination_cup)
    destination_cup -= 1
    destination_cup = 1000000 if destination_cup == 0
  end

  cups[pickup3] = cups[destination_cup]
  cups[destination_cup] = pickup1

  current_cup = cups[current_cup]
end

cup1 = cups[1]
cup2 = cups[cup1]
puts cup1.to_u64 * cup2.to_u64