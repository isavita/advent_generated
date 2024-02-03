
input = File.readlines('input.txt')

reindeer = input.map do |line|
  line.scan(/\d+/).map(&:to_i)
end

time = 2503

distances = reindeer.map do |speed, fly_time, rest_time|
  full_cycles, leftover_time = time.divmod(fly_time + rest_time)
  full_distance = full_cycles * fly_time * speed
  partial_distance = [fly_time, leftover_time].min * speed
  full_distance + partial_distance
end

puts distances.max
