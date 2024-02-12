
file = File.open("input.txt")
total_ribbon = 0

file.each_line do |line|
  dimensions = line.strip.split("x")
  l, w, h = dimensions.map { |d| d.to_i }

  bow = l * w * h
  sides = [l, w, h].sort
  wrap = 2 * sides[0] + 2 * sides[1]

  total_ribbon += bow + wrap
end

puts total_ribbon
