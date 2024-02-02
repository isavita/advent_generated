
require 'set'

def manhattan_distance(p1, p2)
  p1.zip(p2).map { |a, b| (a - b).abs }.sum
end

def find_constellations(points)
  constellations = Set.new

  points.each do |point|
    new_constellation = Set.new([point])

    constellations.each do |constellation|
      if constellation.any? { |c_point| manhattan_distance(c_point, point) <= 3 }
        new_constellation.merge(constellation)
      end
    end

    constellations.delete_if { |constellation| constellation.any? { |c_point| new_constellation.include?(c_point) } }
    constellations.add(new_constellation)
  end

  constellations.size
end

points = File.readlines('input.txt').map { |line| line.split(',').map(&:to_i) }
puts find_constellations(points)
