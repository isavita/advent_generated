require "big"
require "io"

class RatVec3
  property x : BigFloat
  property y : BigFloat
  property z : BigFloat

  def initialize(x : BigFloat, y : BigFloat, z : BigFloat)
    @x = x
    @y = y
    @z = z
  end

  def add(other : RatVec3) : RatVec3
    RatVec3.new(@x + other.x, @y + other.y, @z + other.z)
  end

  def subtract(other : RatVec3) : RatVec3
    RatVec3.new(@x - other.x, @y - other.y, @z - other.z)
  end

  def multiply(s : BigFloat) : RatVec3
    RatVec3.new(@x * s, @y * s, @z * s)
  end

  def divide(s : BigFloat) : RatVec3
    RatVec3.new(@x / s, @y / s, @z / s)
  end

  def cross(other : RatVec3) : RatVec3
    RatVec3.new(
      @y * other.z - @z * other.y,
      @z * other.x - @x * other.z,
      @x * other.y - @y * other.x
    )
  end

  def dot(other : RatVec3) : BigFloat
    @x * other.x + @y * other.y + @z * other.z
  end
end

class HailStone
  property p : RatVec3
  property v : RatVec3

  def initialize(p : RatVec3, v : RatVec3)
    @p = p
    @v = v
  end

  def subtract(other : HailStone) : HailStone
    HailStone.new(@p.subtract(other.p), @v.subtract(other.v))
  end
end

def read_file(file_name : String) : Array(String)
  File.read(file_name).split("\n").map(&.strip)
end

def solve(input : Array(String)) : String
  hail_stones = read_input(input[0..2])
  s1 = hail_stones[1]
  s2 = hail_stones[2]
  ref1 = s1.subtract(hail_stones[0])
  ref2 = s2.subtract(hail_stones[0])

  t1 = intersection_time(ref2, ref1)
  t2 = intersection_time(ref1, ref2)

  rock1 = s1.p.add(s1.v.multiply(t1))
  rock2 = s2.p.add(s2.v.multiply(t2))

  rp = rock1.subtract(rock2.subtract(rock1).divide(t2 - t1).multiply(t1))
  (rp.x + rp.y + rp.z).to_s
end

def read_input(input : Array(String)) : Array(HailStone)
  input.map { |line| read_line(line) }
end

def read_line(line : String) : HailStone
  match = line.scan(/-?\d+/).map(&.to_s) # Convert matches to strings first
  HailStone.new(
    RatVec3.new(BigFloat.new(match[0]), BigFloat.new(match[1]), BigFloat.new(match[2])),
    RatVec3.new(BigFloat.new(match[3]), BigFloat.new(match[4]), BigFloat.new(match[5]))
  )
end

def intersection_time(r : HailStone, s : HailStone) : BigFloat
  plane = r.p.cross(r.p.add(r.v))
  -s.p.dot(plane) / s.v.dot(plane)
end

input = read_file("input.txt")
puts solve(input)