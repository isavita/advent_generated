def abs(x)
  x < 0 ? -x : x
end

def manhattan(x)
  abs(x[0]) + abs(x[1]) + abs(x[2])
end

particles = [] of Tuple(Array(Int32), Array(Int32), Array(Int32))
File.each_line("input.txt") do |line|
  parts = line.split(", ")
  p = [] of Array(Int32)
  parts.each do |part|
    p << part[3..-2].split(",").map(&.to_i)
  end
  particles << {p[0], p[1], p[2]}
end

closest_particle = 0
min_accel = Int32::MAX
min_velocity = Int32::MAX
min_position = Int32::MAX

particles.each_with_index do |particle, i|
  accel = manhattan(particle[2])
  velocity = manhattan(particle[1])
  position = manhattan(particle[0])

  if accel < min_accel || (accel == min_accel && velocity < min_velocity) ||
     (accel == min_accel && velocity == min_velocity && position < min_position)
    min_accel = accel
    min_velocity = velocity
    min_position = position
    closest_particle = i
  end
end

puts closest_particle