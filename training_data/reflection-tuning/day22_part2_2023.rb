require 'set'

class Brick
  attr_reader :id, :x1, :y1, :z1, :x2, :y2, :z2
  attr_accessor :supporting, :supported_by

  def initialize(id, x1, y1, z1, x2, y2, z2)
    @id, @x1, @y1, @z1, @x2, @y2, @z2 = id, x1, y1, z1, x2, y2, z2
    @supporting, @supported_by = Set.new, Set.new
  end

  def fall(distance)
    @z1 -= distance
    @z2 -= distance
  end

  def overlaps?(other)
    @x1 <= other.x2 && @x2 >= other.x1 &&
    @y1 <= other.y2 && @y2 >= other.y1
  end
end

def parse_input(filename)
  File.readlines(filename).map.with_index do |line, i|
    coords = line.scan(/\d+/).map(&:to_i)
    Brick.new(i, *coords)
  end.sort_by { |b| [b.z1, b.z2] }
end

def simulate_fall(bricks)
  bricks.each do |brick|
    max_z = 1
    bricks.each do |other|
      next if other == brick || other.z2 >= brick.z1
      max_z = [max_z, other.z2 + 1].max if brick.overlaps?(other)
    end
    brick.fall(brick.z1 - max_z)
  end
end

def build_support_graph(bricks)
  bricks.combination(2) do |a, b|
    if a.z2 + 1 == b.z1 && a.overlaps?(b)
      a.supporting.add(b)
      b.supported_by.add(a)
    elsif b.z2 + 1 == a.z1 && b.overlaps?(a)
      b.supporting.add(a)
      a.supported_by.add(b)
    end
  end
end

def count_falling_bricks(brick, fallen)
  fallen.add(brick)
  brick.supporting.each do |supported|
    count_falling_bricks(supported, fallen) if (supported.supported_by - fallen).empty?
  end
  fallen.size - 1
end

bricks = parse_input("input.txt")
simulate_fall(bricks)
build_support_graph(bricks)

part1 = bricks.count { |brick| brick.supporting.all? { |s| s.supported_by.size > 1 } }
part2 = bricks.sum { |brick| count_falling_bricks(brick, Set.new) }

puts "Part 1: #{part1}"
puts "Part 2: #{part2}"
