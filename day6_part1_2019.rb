
input = File.readlines("input.txt").map(&:chomp)

orbit_map = {}
input.each do |line|
  center, orbiter = line.split(')')
  orbit_map[orbiter] = center
end

total_orbits = 0
orbit_map.keys.each do |orbiter|
  current = orbiter
  while orbit_map[current]
    total_orbits += 1
    current = orbit_map[current]
  end
end

puts total_orbits
