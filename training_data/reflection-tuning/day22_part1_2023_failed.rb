class Brick
  attr_reader :x1, :y1, :z1, :x2, :y2, :z2
  attr_accessor :supporting, :supported_by

  def initialize(x1, y1, z1, x2, y2, z2)
    @x1, @y1, @z1 = x1, y1, z1
    @x2, @y2, @z2 = x2, y2, z2
    @supporting = []
    @supported_by = []
  end

  def fall(distance)
    @z1 -= distance
    @z2 -= distance
  end

  def cubes
    (x1..x2).flat_map { |x| (y1..y2).flat_map { |y| (z1..z2).map { |z| [x, y, z] } } }
  end
end

def parse_input(filename)
  File.readlines(filename).map do |line|
    coords = line.strip.split('~').flat_map { |part| part.split(',').map(&:to_i) }
    Brick.new(*coords)
  end
end

def simulate_fall(bricks)
  bricks.sort_by!(&:z1)
  grid = {}

  bricks.each do |brick|
    fall_distance = brick.z1 - 1
    brick.cubes.each do |x, y, z|
      max_z = (1...z).reverse_each.find { |check_z| grid[[x, y, check_z]] } || 0
      fall_distance = [fall_distance, z - max_z - 1].min
    end

    brick.fall(fall_distance)
    brick.cubes.each { |cube| grid[cube] = brick }

    brick.cubes.each do |x, y, z|
      below = grid[[x, y, z - 1]]
      if below && below != brick
        below.supporting << brick
        brick.supported_by << below
      end
    end
  end
end

def count_safe_to_disintegrate(bricks)
  bricks.count do |brick|
    brick.supporting.all? { |supported| supported.supported_by.size > 1 }
  end
end

bricks = parse_input("input.txt")
simulate_fall(bricks)
result = count_safe_to_disintegrate(bricks)
puts result
