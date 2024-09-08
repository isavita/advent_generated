require 'set'

def parse_input(filename)
  active_cubes = Set.new
  File.readlines(filename, chomp: true).each_with_index do |line, y|
    line.chars.each_with_index do |char, x|
      active_cubes.add([x, y, 0]) if char == '#'
    end
  end
  active_cubes
end

def neighbors(x, y, z)
  (-1..1).flat_map do |dx|
    (-1..1).flat_map do |dy|
      (-1..1).map do |dz|
        [x + dx, y + dy, z + dz] unless dx == 0 && dy == 0 && dz == 0
      end
    end
  end.compact
end

def simulate_cycle(active_cubes)
  neighbor_counts = Hash.new(0)
  active_cubes.each do |cube|
    neighbors(*cube).each do |neighbor|
      neighbor_counts[neighbor] += 1
    end
  end

  new_active_cubes = Set.new
  neighbor_counts.each do |cube, count|
    if count == 3 || (count == 2 && active_cubes.include?(cube))
      new_active_cubes.add(cube)
    end
  end
  new_active_cubes
end

active_cubes = parse_input('input.txt')
6.times do
  active_cubes = simulate_cycle(active_cubes)
end

puts active_cubes.size
