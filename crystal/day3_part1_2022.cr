require "char"
require "file"

file = File.open("input.txt")
sum = 0

file.each_line do |line|
  half = line.size // 2
  first_compartment = line[0, half].chars
  second_compartment = line[half, line.size - half].chars

  compartment_map = Hash(Char, Int32).new
  first_compartment.each do |item|
    compartment_map[item] = 1
  end

  second_compartment.each do |item|
    if compartment_map.has_key?(item)
      if 'a' <= item && item <= 'z'
        sum += item - 'a' + 1
      else
        sum += item - 'A' + 27
      end
      break
    end
  end
end

puts sum