file = File.open("input.txt")
adapters = [0]
file.each_line { |line| adapters << line.to_i }
adapters.sort!
adapters << adapters.last + 3

ways = Hash(Int32, Int64).new(0)
ways[0] = 1

adapters.each_with_index { |joltage, i| 
  if i > 0
    [1, 2, 3].each { |diff| ways[joltage] += ways[joltage - diff] if ways[joltage - diff]? }
  end
}

puts ways[adapters.last]