
input = File.readlines('input.txt').map(&:chomp)

count = 0
input.each do |line|
  range1, range2 = line.split(',').map { |r| r.split('-').map(&:to_i) }
  count += 1 if (range1[0] <= range2[0] && range1[1] >= range2[1]) || (range2[0] <= range1[0] && range2[1] >= range1[1])
end

puts count
