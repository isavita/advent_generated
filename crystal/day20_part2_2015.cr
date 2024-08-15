File.read("input.txt").strip.to_i.tap do |target|
  target = (target / 11).to_i
  houses = Array.new(target + 1, 0)

  (1..target).each do |elf|
    (elf..[elf * 50, target].min).step(elf) do |house|
      houses[house] += elf
    end
  end

  houses.index { |presents| presents >= target }.tap { |house_number| puts house_number }
end