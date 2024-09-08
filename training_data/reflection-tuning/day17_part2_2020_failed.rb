require 'set'

def simulate_cycles(initial_state, cycles, dimensions)
  active_cubes = Set.new
  initial_state.each_with_index do |row, y|
    row.each_char.with_index do |cell, x|
      active_cubes.add([x, y, 0, 0]) if cell == '#'
    end
  end

  cycles.times do
    new_active_cubes = Set.new
    to_check = Set.new

    active_cubes.each do |cube|
      neighbors(cube, dimensions).each { |neighbor| to_check.add(neighbor) }
    end

    to_check.each do |cube|
      active_neighbors = neighbors(cube, dimensions).count { |neighbor| active_cubes.include?(neighbor) }
      if active_cubes.include?(cube)
        new_active_cubes.add(cube) if active_neighbors == 2 || active_neighbors == 3
      else
        new_active_cubes.add(cube) if active_neighbors == 3
      end
    end

    active_cubes = new_active_cubes
  end

  active_cubes.size
end

def neighbors(coord, dimensions)
  deltas = [-1, 0, 1].repeated_permutation(dimensions).to_a - [[0] * dimensions]
  deltas.map do |delta|
    coord.zip(delta).map { |a, b| a + b }
  end
end

initial_state = File.readlines('input.txt').map(&:chomp)

puts "Part 1: #{simulate_cycles(initial_state, 6, 3)}"
puts "Part 2: #{simulate_cycles(initial_state, 6, 4)}"
