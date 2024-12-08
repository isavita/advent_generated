
sum = 0
File.read("input.txt").scan(/mul\((\d+),(\d+)\)/).each do |match|
  x, y = match.map(&:to_i)
  sum += x * y
end
puts sum
