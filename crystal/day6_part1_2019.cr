
data = File.read("input.txt").chomp
lines = data.split("\n")
orbit_map = Hash(String, Array(String)).new

lines.each do |line|
  parts = line.split(")")
  center, orbiter = parts[0], parts[1]
  orbit_map[center] ||= [] of String
  orbit_map[center] << orbiter
end

def count_orbits(orbit_map, start, depth)
  orbits = orbit_map[start]?
  return depth unless orbits
  count = depth
  orbits.each do |orbit|
    count += count_orbits(orbit_map, orbit, depth + 1)
  end
  count
end

total_orbits = count_orbits(orbit_map, "COM", 0)
puts total_orbits
