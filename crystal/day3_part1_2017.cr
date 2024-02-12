
data = File.read("input.txt").chomp.to_i

side_length = Math.sqrt(data).ceil.to_i
side_length += 1 if side_length.even?

max_value = side_length * side_length
steps_from_edge = (side_length - 1) / 2
distance_to_middle = nil

4.times do |i|
  middle_point = max_value - steps_from_edge - (side_length - 1) * i
  distance = (data - middle_point).abs
  distance_to_middle = distance if distance_to_middle.nil? || distance < distance_to_middle
end

manhattan_distance = steps_from_edge + distance_to_middle.not_nil!

puts manhattan_distance
