
input = File.read('input.txt').split("\n")

total_priority = 0

input.each do |line|
  first_half = line[0, line.length / 2]
  second_half = line[line.length / 2, line.length / 2]

  common_items = first_half.chars & second_half.chars

  common_items.each do |item|
    total_priority += item.downcase.ord - 'a'.ord + 1 if item =~ /[a-z]/
    total_priority += item.upcase.ord - 'A'.ord + 27 if item =~ /[A-Z]/
  end
end

puts total_priority
