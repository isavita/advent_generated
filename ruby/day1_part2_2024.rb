
left, right = [], []
File.open("input.txt").each_with_index do |line, i|
  l, r = line.split.map(&:to_i)
  left << l
  right << r
end

right_count = right.tally

similarity_score = left.sum { |num| right_count.fetch(num, 0) * num }

puts similarity_score
