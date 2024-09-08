target = gets.to_i
presents = Hash.new(0)
house = 0

(1..Float::INFINITY).each do |elf|
  (elf..Float::INFINITY).step(elf) do |house|
    presents[house] += elf * 10
    if presents[house] >= target
      puts house
      exit
    end
  end
end
