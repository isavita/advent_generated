require "file_utils"

struct Coordinate
  getter x, y, z

  def initialize(@x : Int32, @y : Int32, @z : Int32)
  end
end

def simulate_cycle(active_cubes)
  new_active_cubes = {} of Coordinate => Bool
  neighbor_counts = {} of Coordinate => Int32

  active_cubes.each_key do |coord|
    (-1..1).each do |dz|
      (-1..1).each do |dy|
        (-1..1).each do |dx|
          next if dz == 0 && dy == 0 && dx == 0
          neighbor = Coordinate.new(coord.x + dx, coord.y + dy, coord.z + dz)
          neighbor_counts[neighbor] = neighbor_counts.fetch(neighbor, 0) + 1
        end
      end
    end
  end

  neighbor_counts.each do |coord, count|
    if count == 3 || (count == 2 && active_cubes[coord]?)
      new_active_cubes[coord] = true
    end
  end

  new_active_cubes
end

input = File.read("input.txt").strip.split("\n")
active_cubes = {} of Coordinate => Bool

input.each_with_index do |line, y|
  line.each_char_with_index do |char, x|
    if char == '#'
      active_cubes[Coordinate.new(x, y, 0)] = true
    end
  end
end

6.times do
  active_cubes = simulate_cycle(active_cubes)
end

puts active_cubes.size