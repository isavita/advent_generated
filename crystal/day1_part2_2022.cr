require "csv"

input = File.read("input.txt")

data = input.split("\n\n").map do |elf|
  elf.split("\n").map{|line| line.to_i}.sum
end

sorted_data = data.sort.reverse.first(3).sum

puts sorted_data