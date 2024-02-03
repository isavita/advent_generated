
def find_house(target, part_two = false)
  houses = Array.new(target / 10, 0)
  (1..target / 10).each do |elf|
    limit = part_two ? 50 : target / 10
    (elf..[target / 10, elf * limit].min).step(elf).each_with_index do |house, index|
      break if part_two && index >= 50
      houses[house - 1] += part_two ? elf * 11 : elf * 10
    end
  end
  houses.index { |presents| presents >= target } + 1
end

input = File.read("input.txt").to_i
puts find_house(input)
puts find_house(input, true)
