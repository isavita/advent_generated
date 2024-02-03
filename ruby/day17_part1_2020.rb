
require 'set'

ACTIVE = '#'
INACTIVE = '.'

def count_active_neighbors(grid, x, y, z)
  count = 0
  (-1..1).each do |dz|
    (-1..1).each do |dy|
      (-1..1).each do |dx|
        next if dx == 0 && dy == 0 && dz == 0
        count += 1 if grid.include?([x + dx, y + dy, z + dz])
      end
    end
  end
  count
end

def simulate_cycle(active_cubes)
  new_active_cubes = Set.new
  inactive_neighbors = Hash.new(0)

  active_cubes.each do |x, y, z|
    active_neighbors = count_active_neighbors(active_cubes, x, y, z)
    if active_neighbors == 2 || active_neighbors == 3
      new_active_cubes.add([x, y, z])
    end

    (-1..1).each do |dz|
      (-1..1).each do |dy|
        (-1..1).each do |dx|
          next if dx == 0 && dy == 0 && dz == 0
          inactive_neighbors[[x + dx, y + dy, z + dz]] += 1
        end
      end
    end
  end

  inactive_neighbors.each do |coords, count|
    new_active_cubes.add(coords) if count == 3
  end

  new_active_cubes
end

input = File.read('input.txt').split("\n").map(&:chars)

active_cubes = Set.new
input.each_with_index do |row, y|
  row.each_with_index do |val, x|
    active_cubes.add([x, y, 0]) if val == ACTIVE
  end
end

6.times do
  active_cubes = simulate_cycle(active_cubes)
end

puts active_cubes.size
