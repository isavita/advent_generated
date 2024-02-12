
file = File.open("input.txt")
count = 0
prev = 0

file.each_line do |line|
  current = line.to_i
  count += 1 if prev != 0 && current > prev
  prev = current
end

puts count
