
input = File.readlines("input.txt").map(&:chomp)

def find_common_items(rucksacks)
  common_items = []
  rucksacks.each_slice(3) do |group|
    common_items << group.map { |rucksack| rucksack.split('') }.inject(:&).join('')
  end
  common_items
end

def calculate_priority_sum(items)
  sum = 0
  items.each do |item|
    sum += item.downcase.ord - 96 if item =~ /[a-z]/
    sum += item.ord - 38 if item =~ /[A-Z]/
  end
  sum
end

common_items = find_common_items(input)
puts calculate_priority_sum(common_items)
