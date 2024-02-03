
input = File.readlines('input.txt').map(&:chomp)

cubes = input.map { |line| line.split(',').map(&:to_i) }

adjacent_cubes = [[-1, 0, 0], [1, 0, 0], [0, -1, 0], [0, 1, 0], [0, 0, -1], [0, 0, 1]]

surface_area = cubes.sum do |cube|
  adjacent_cubes.count { |adj_cube| !cubes.include?(cube.map.with_index { |coord, i| coord + adj_cube[i] }) }
end

puts surface_area
