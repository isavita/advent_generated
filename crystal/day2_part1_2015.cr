
file = File.open("input.txt")
total = 0

file.each_line do |line|
  dimensions = line.strip.split("x")
  l = dimensions[0].to_i
  w = dimensions[1].to_i
  h = dimensions[2].to_i

  side1 = l * w
  side2 = w * h
  side3 = h * l

  smallest = [side1, side2, side3].min
  total += 2 * side1 + 2 * side2 + 2 * side3 + smallest
end

puts total
