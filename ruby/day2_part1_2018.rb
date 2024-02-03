
input = File.readlines('input.txt').map(&:chomp)

twos = 0
threes = 0

input.each do |id|
  counts = id.chars.group_by(&:itself).transform_values(&:count).values.uniq
  twos += 1 if counts.include?(2)
  threes += 1 if counts.include?(3)
end

puts twos * threes
