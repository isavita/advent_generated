def part_one(target)
  house = 1
  loop do
    presents = (1..Math.sqrt(house)).sum do |i|
      if house % i == 0
        i == house / i ? i * 10 : (i + house / i) * 10
      else
        0
      end
    end
    return house if presents >= target
    house += 1
  end
end

def part_two(target)
  house = 1
  elf_visits = Hash.new(0)
  loop do
    presents = (1..Math.sqrt(house)).sum do |i|
      if house % i == 0
        sum = 0
        [i, house / i].uniq.each do |elf|
          if elf_visits[elf] < 50
            sum += elf * 11
            elf_visits[elf] += 1
          end
        end
        sum
      else
        0
      end
    end
    return house if presents >= target
    house += 1
  end
end

target = File.read('input.txt').to_i

puts "Part One: #{part_one(target)}"
puts "Part Two: #{part_two(target)}"
