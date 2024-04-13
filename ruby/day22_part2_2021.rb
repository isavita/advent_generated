class Cube
  attr_accessor :is_on, :x1, :x2, :y1, :y2, :z1, :z2

  def initialize(is_on, x1, x2, y1, y2, z1, z2)
    @is_on = is_on
    @x1, @x2 = x1, x2
    @y1, @y2 = y1, y2
    @z1, @z2 = z1, z2
  end

  def get_intersection(other)
    x1 = [self.x1, other.x1].max
    x2 = [self.x2, other.x2].min
    y1 = [self.y1, other.y1].max
    y2 = [self.y2, other.y2].min
    z1 = [self.z1, other.z1].max
    z2 = [self.z2, other.z2].min

    return nil if x1 > x2 || y1 > y2 || z1 > z2

    intersection_state = if self.is_on && other.is_on
                           false
                         elsif !self.is_on && !other.is_on
                           true
                         else
                           other.is_on
                         end

    Cube.new(intersection_state, x1, x2, y1, y2, z1, z2)
  end

  def volume
    vol = (self.x2 - self.x1 + 1) * (self.y2 - self.y1 + 1) * (self.z2 - self.z1 + 1)
    self.is_on ? vol : -vol
  end
end

def parse_input(input)
  cubes = []
  input.each_line do |line|
    parts = line.split
    coords = parts[1].scan(/-?\d+/).map(&:to_i)
    cubes << Cube.new(parts[0] == "on", *coords)
  end
  cubes
end

def solve(input)
  cubes = parse_input(input)
  final_list = []

  cubes.each do |c|
    to_add = []

    final_list.each do |final_cube|
      intersection = final_cube.get_intersection(c)
      to_add << intersection if intersection
    end

    to_add << c if c.is_on

    final_list += to_add
  end

  final_list.sum(&:volume)
end

input = File.read('input.txt').strip
puts solve(input)