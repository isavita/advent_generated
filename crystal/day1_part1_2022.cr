
file = File.open("input.txt")
max_calories = 0
current_calories = 0

file.each_line do |line|
  if line.strip.empty?
    max_calories = current_calories if current_calories > max_calories
    current_calories = 0
    next
  end

  calories = line.to_i
  current_calories += calories
end

max_calories = current_calories if current_calories > max_calories

puts max_calories
