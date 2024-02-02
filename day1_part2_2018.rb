
frequencies = File.readlines('input.txt').map(&:to_i)
puts "Part One: #{frequencies.sum}"

current_frequency = 0
seen_frequencies = {0 => true}
duplicate_frequency = nil

loop do
  frequencies.each do |freq|
    current_frequency += freq
    if seen_frequencies[current_frequency]
      duplicate_frequency = current_frequency
      break
    end
    seen_frequencies[current_frequency] = true
  end
  break if duplicate_frequency
end

puts "Part Two: #{duplicate_frequency}"
