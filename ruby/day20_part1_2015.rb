
input = File.read("input.txt").to_i

def find_house(target)
  presents = Array.new(target / 10, 0)
  (1..target / 10).each do |elf|
    (elf..target / 10).step(elf) do |house|
      presents[house - 1] += elf * 10
    end
  end
  presents.index { |p| p >= target } + 1
end

puts find_house(input)
