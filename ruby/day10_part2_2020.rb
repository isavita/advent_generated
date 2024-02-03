
adapters = File.readlines('input.txt').map(&:to_i).sort
adapters.unshift(0)
adapters.push(adapters.last + 3)

diffs = adapters.each_cons(2).map { |a, b| b - a }
puts diffs.count(1) * diffs.count(3)

ways = Hash.new(0)
ways[0] = 1

adapters.each do |adapter|
  ways[adapter] += ways[adapter - 1] + ways[adapter - 2] + ways[adapter - 3]
end

puts ways[adapters.last]
