File.open("input.txt", "r") do |file|
  line = file.gets
  if line
    banks = line.split.map(&.to_i)
    seen = Set(typeof(banks)).new
    cycles = 0

    while !seen.includes?(banks)
      seen.add(banks.dup)
      max = banks.max
      idx = banks.index(max).not_nil!
      banks[idx] = 0
      (1..max).each do |i|
        banks[(idx + i) % banks.size] += 1
      end
      cycles += 1
    end

    puts cycles
  else
    puts "Error: Input file is empty."
  end
end