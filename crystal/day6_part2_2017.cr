File.open("input.txt", "r") do |file|
  banks = file.gets.not_nil!.split.map(&.to_i)
  seen = {} of String => Int32
  cycles = 0

  loop do
    state = banks.to_s
    if seen.has_key?(state)
      puts "The size of the loop is #{cycles - seen[state]}"
      break
    end
    seen[state] = cycles

    max_value = banks.max
    max_index = banks.index(max_value).not_nil!
    blocks = max_value
    banks[max_index] = 0
    (1..blocks).each do |i|
      banks[(max_index + i) % banks.size] += 1
    end

    cycles += 1
  end
end