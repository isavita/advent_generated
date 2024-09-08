def find_house(target, multiplier, limit = nil)
  max_house = target / multiplier
  presents = Array.new(max_house + 1, 0)

  (1..max_house).each do |elf|
    house = elf
    count = 0
    while house <= max_house && (limit.nil? || count < limit)
      presents[house] += elf * multiplier
      house += elf
      count += 1
    end
  end

  presents.each_with_index do |p, i|
    return i if i > 0 && p >= target
  end
end

input = 29000000  # Replace with your actual input

# Part One
puts "Part One: #{find_house(input, 10)}"

# Part Two
puts "Part Two: #{find_house(input, 11, 50)}"
