class Coord
  attr_accessor :x, :y, :z

  def initialize(x, y, z)
    @x, @y, @z = x, y, z
  end
end

class Point
  attr_accessor :pos, :vel

  def initialize(pos, vel)
    @pos = pos
    @vel = vel
  end
end

def parse_input(input)
  points = []
  input.each do |line|
    pos_x, pos_y, pos_z, vel_x, vel_y, vel_z = line.scan(/-?\d+/).map(&:to_f)
    points << Point.new(Coord.new(pos_x, pos_y, pos_z), Coord.new(vel_x, vel_y, vel_z))
  end
  points
end

def is_intersecting_2d(p1, p2)
  det = p1.vel.x * p2.vel.y - p2.vel.x * p1.vel.y
  return false, Coord.new(0, 0, 0), 0, 0 if det.zero?

  t1 = (p2.vel.y * (p2.pos.x - p1.pos.x) - p2.vel.x * (p2.pos.y - p1.pos.y)) / det
  t2 = (p1.vel.y * (p2.pos.x - p1.pos.x) - p1.vel.x * (p2.pos.y - p1.pos.y)) / det
  coord = Coord.new(p1.pos.x + p1.vel.x * t1, p1.pos.y + p1.vel.y * t1, 0)
  [true, coord, t1, t2]
end

def solve(input, min, max)
  points = parse_input(input)
  count = 0

  points.each_with_index do |p1, i|
    (0...i).each do |j|
      p2 = points[j]
      is_intersecting, coord, time1, time2 = is_intersecting_2d(p1, p2)
      is_in_bound = (min <= coord.x && coord.x <= max) && (min <= coord.y && coord.y <= max)
      count += 1 if is_intersecting && is_in_bound && time1 >= 0 && time2 >= 0
    end
  end

  count
end

input = File.readlines('input.txt', chomp: true)
puts solve(input, 200_000_000_000_000, 400_000_000_000_000)