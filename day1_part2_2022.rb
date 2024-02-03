
calories_list = []
current_calories = 0

File.open("input.txt").each do |line|
  line = line.chomp

  if line == ""
    calories_list << current_calories
    current_calories = 0
    next
  end

  calories = line.to_i
  current_calories += calories
end

calories_list << current_calories
calories_list.sort!.reverse!

top_three_sum = 0
calories_list.first(3).each do |calories|
  top_three_sum += calories
end

puts top_three_sum
