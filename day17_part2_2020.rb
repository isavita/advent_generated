
require 'set'

ACTIVE = '#'
INACTIVE = '.'

def count_active_neighbors(hypercube, x, y, z, w)
  count = 0
  (-1..1).each do |dw|
    (-1..1).each do |dz|
      (-1..1).each do |dy|
        (-1..1).each do |dx|
          next if dx == 0 && dy == 0 && dz == 0 && dw == 0
          count += 1 if hypercube.include?([x + dx, y + dy, z + dz, w + dw])
        end
      end
    end
  end
  count
end

def simulate_cycle(hypercube)
  new_hypercube = Set.new
  x_range = (hypercube.map { |x, _, _, _| x }.min - 1..hypercube.map { |x, _, _, _| x }.max + 1)
  y_range = (hypercube.map { |_, y, _, _| y }.min - 1..hypercube.map { |_, y, _, _| y }.max + 1)
  z_range = (hypercube.map { |_, _, z, _| z }.min - 1..hypercube.map { |_, _, z, _| z }.max + 1)
  w_range = (hypercube.map { |_, _, _, w| w }.min - 1..hypercube.map { |_, _, _, w| w }.max + 1)

  x_range.each do |x|
    y_range.each do |y|
      z_range.each do |z|
        w_range.each do |w|
          active_neighbors = count_active_neighbors(hypercube, x, y, z, w)
          if hypercube.include?([x, y, z, w])
            new_hypercube << [x, y, z, w] if active_neighbors.between?(2, 3)
          else
            new_hypercube << [x, y, z, w] if active_neighbors == 3
          end
        end
      end
    end
  end

  new_hypercube
end

hypercube = Set.new
File.open('input.txt').each_with_index do |line, y|
  line.strip.chars.each_with_index do |char, x|
    hypercube << [x, y, 0, 0] if char == ACTIVE
  end
end

6.times do
  hypercube = simulate_cycle(hypercube)
end

puts hypercube.size
