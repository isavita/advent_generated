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

  def overlaps?(other)
    !(other.x2 < x1 || other.x1 > x2 || other.y2 < y1 || other.y1 > y2)
  end
end

def parse_input(input)
  input.map do |line|
    coords = line.scan(/\d+/).map(&:to_i)
    Brick.new(*coords)
  end
end

def settle_bricks(bricks)
  bricks.sort_by! { |b| [b.z1, b.z2] }
  settled = []

  bricks.each do |brick|
    max_z = 1
    supporters = []

    settled.each do |settled_brick|
      if brick.overlaps?(settled_brick)
        if settled_brick.z2 + 1 > max_z
          max_z = settled_brick.z2 + 1
          supporters = [settled_brick]
        elsif settled_brick.z2 + 1 == max_z
          supporters << settled_brick
        end
      end
    end

    fall_distance = brick.z1 - max_z
    brick.fall(fall_distance)

    supporters.each do |supporter|
      supporter.supporting << brick
      brick.supported_by << supporter
    end

    settled << brick
  end

  settled
end

def count_safe_to_disintegrate(bricks)
  bricks.count do |brick|
    brick.supporting.all? { |supported| supported.supported_by.size > 1 }
  end
end

input = File.readlines('input.txt').map(&:chomp)
bricks = parse_input(input)
settled_bricks = settle_bricks(bricks)
result = count_safe_to_disintegrate(settled_bricks)
puts result
