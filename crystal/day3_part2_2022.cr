def priority(item : Char)
  if item.downcase == item
    item.ord - 96
  else
    item.ord - 38
  end
end

def find_common_item(comp1, comp2)
  (comp1.chars & comp2.chars).first
end

def find_common_item_in_three(ruck1, ruck2, ruck3)
  (ruck1.chars & ruck2.chars & ruck3.chars).first
end

# Part 1
sum = 0
File.each_line("input.txt") do |line|
  midpoint = line.size // 2
  comp1 = line[0, midpoint]
  comp2 = line[midpoint, line.size - midpoint]
  common_item = find_common_item(comp1, comp2)
  sum += priority(common_item)
end
puts "Part 1: #{sum}"

# Part 2
sum = 0
rucksacks = File.read_lines("input.txt")
rucksacks.each_slice(3) do |group|
  common_item = find_common_item_in_three(group[0], group[1], group[2])
  sum += priority(common_item)
end
puts "Part 2: #{sum}"