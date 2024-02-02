total_calories = 0
max_calories = 0

File.open("input.txt").each do |line|
  if line.strip.empty?
    max_calories = [max_calories, total_calories].max
    total_calories = 0
  else
    total_calories += line.to_i
  end
end

puts max_calories