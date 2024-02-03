
require 'bigdecimal'

class RatVec3
  attr_accessor :x, :y, :z

  def initialize(x, y, z)
    @x = BigDecimal(x)
    @y = BigDecimal(y)
    @z = BigDecimal(z)
  end

  def add(other)
    RatVec3.new(@x + other.x, @y + other.y, @z + other.z)
  end

  def subtract(other)
    RatVec3.new(@x - other.x, @y - other.y, @z - other.z)
  end

  def multiply(s)
    RatVec3.new(@x * s, @y * s, @z * s)
  end

  def divide(s)
    RatVec3.new(@x / s, @y / s, @z / s)
  end

  def cross(other)
    RatVec3.new(@y * other.z - @z * other.y, @z * other.x - @x * other.z, @x * other.y - @y * other.x)
  end

  def dot(other)
    @x * other.x + @y * other.y + @z * other.z
  end
end

class HailStone
  attr_accessor :p, :v

  def initialize(p, v)
    @p = p
    @v = v
  end

  def subtract(other)
    HailStone.new(@p.subtract(other.p), @v.subtract(other.v))
  end
end

def read_input(file)
  File.readlines(file).map(&:chomp)
end

def solve(input)
  hail_stones = read_input(input)[0..2].map { |line| read_line(line) }
  s1 = hail_stones[1]
  s2 = hail_stones[2]
  ref1 = s1.subtract(hail_stones[0])
  ref2 = s2.subtract(hail_stones[0])

  t1 = intersection_time(ref2, ref1)
  t2 = intersection_time(ref1, ref2)

  rock1 = s1.p.add(s1.v.multiply(t1))
  rock2 = s2.p.add(s2.v.multiply(t2))

  rp = rock1.subtract(rock2.subtract(rock1).divide(t2 - t1).multiply(t1))
  (rp.x + rp.y + rp.z).to_i.to_s
end

def read_line(line)
  m = line.scan(/-*\d+/).map(&:to_i)
  HailStone.new(RatVec3.new(m[0], m[1], m[2]), RatVec3.new(m[3], m[4], m[5]))
end

def intersection_time(r, s)
  plane = r.p.cross(r.p.add(r.v))
  (s.p.dot(plane) * -1) / s.v.dot(plane)
end

puts solve('input.txt')
