file = File.new("input.txt", "r")
input = file.gets_to_end.chomp
target = (input.to_i / 10.0).ceil.to_i

houses = Array.new(target + 1, 0)

(1..target).each do |elf|
  (elf..target).step(elf) do |house|
    houses[house] += elf
  end
end

houses.each_with_index do |presents, house_number|
  if presents >= target
    puts house_number
    break
  end
end