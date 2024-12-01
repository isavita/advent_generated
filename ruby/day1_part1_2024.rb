
left, right = File.readlines('input.txt').map { |line| line.split.map(&:to_i) }.transpose

raise "Lists have different lengths" unless left.length == right.length

left.sort!
right.sort!

total_distance = left.zip(right).sum { |l, r| (l - r).abs }

puts total_distance
