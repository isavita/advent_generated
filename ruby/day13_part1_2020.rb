
input = File.readlines('input.txt').map(&:chomp)
timestamp = input[0].to_i
buses = input[1].split(',').reject { |bus| bus == 'x' }.map(&:to_i)

bus_id = buses.min_by { |bus| (timestamp / bus + 1) * bus }
puts bus_id * ((timestamp / bus_id + 1) * bus_id - timestamp)
