class Nanobot
  attr_reader :x, :y, :z, :r

  def initialize(x, y, z, r)
    @x, @y, @z, @r = x, y, z, r
  end

  def distance_to(other)
    (@x - other.x).abs + (@y - other.y).abs + (@z - other.z).abs
  end
end

def parse_input(input)
  input.map do |line|
    match = line.match(/pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/)
    Nanobot.new(*match[1..4].map(&:to_i))
  end
end

def solve(input)
  nanobots = parse_input(input)
  strongest = nanobots.max_by(&:r)
  
  in_range = nanobots.count do |nanobot|
    strongest.distance_to(nanobot) <= strongest.r
  end

  in_range
end

# Example usage:
input = [
  "pos=<0,0,0>, r=4",
  "pos=<1,0,0>, r=1",
  "pos=<4,0,0>, r=3",
  "pos=<0,2,0>, r=1",
  "pos=<0,5,0>, r=3",
  "pos=<0,0,3>, r=1",
  "pos=<1,1,1>, r=1",
  "pos=<1,1,2>, r=1",
  "pos=<1,3,1>, r=1"
]

puts solve(input)
