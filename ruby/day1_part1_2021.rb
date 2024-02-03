depths = File.readlines('input.txt').map(&:to_i)
count = 0

depths.each_with_index do |depth, index|
  count += 1 if index > 0 && depth > depths[index - 1]
end

puts count