
input = File.readlines('input.txt').map(&:chomp)
buses = input[1].split(',').map.with_index { |bus, idx| [bus.to_i, idx] }.reject { |bus, _| bus.zero? }

timestamp = 0
step = 1

buses.each do |bus, offset|
  timestamp += step while (timestamp + offset) % bus != 0
  step *= bus
end

puts timestamp
