
input = File.read("input.txt")
total_sum = 0
input.scan(/mul\((\d{1,3}),(\d{1,3})\)/) do |match|
  x = match[1].to_i
  y = match[2].to_i
  total_sum += x * y
end
puts total_sum
