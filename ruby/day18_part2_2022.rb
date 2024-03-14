require 'set'

def main
  cubes = Set.new
  neighbors = [
    [-1, 0, 0],
    [1, 0, 0],
    [0, -1, 0],
    [0, 1, 0],
    [0, 0, -1],
    [0, 0, 1]
  ]
  min = [Float::INFINITY, Float::INFINITY, Float::INFINITY]
  max = [-Float::INFINITY, -Float::INFINITY, -Float::INFINITY]

  File.readlines('input.txt').each do |line|
    next if line.strip.empty?
    x, y, z = line.strip.split(',').map(&:to_i)
    cubes.add([x, y, z])
    min[0] = [min[0], x].min
    min[1] = [min[1], y].min
    min[2] = [min[2], z].min
    max[0] = [max[0], x].max
    max[1] = [max[1], y].max
    max[2] = [max[2], z].max
  end

  min[0] -= 1
  min[1] -= 1
  min[2] -= 1
  max[0] += 1
  max[1] += 1
  max[2] += 1

  faces = 0
  queue = [min]
  seen = Set.new([min])

  until queue.empty?
    curr = queue.shift
    neighbors.each do |delta|
      next_cube = [curr[0] + delta[0], curr[1] + delta[1], curr[2] + delta[2]]
      next if next_cube[0] < min[0] || next_cube[1] < min[1] || next_cube[2] < min[2] || next_cube[0] > max[0] || next_cube[1] > max[1] || next_cube[2] > max[2]
      if cubes.include?(next_cube)
        faces += 1
      elsif !seen.include?(next_cube)
        seen.add(next_cube)
        queue.push(next_cube)
      end
    end
  end

  puts faces
end

main