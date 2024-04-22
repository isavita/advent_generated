file = File.open("input.txt")
total_points = 0
file.each_line do |line|
  parts = line.split(" | ")
  winning_numbers = parts[0].split.map { |x| x.to_i rescue 0 }
  your_numbers = parts[1].split.map { |x| x.to_i rescue 0 }
  total_points += calculate_points(winning_numbers, your_numbers)
end
puts total_points

def calculate_points(winning_numbers, your_numbers)
  points = 0
  your_numbers.each do |num|
    if winning_numbers.includes? num
      points = points == 0 ? 1 : points * 2
    end
  end
  points
end