require "file_utils"

struct Pt3
  property x, y, z

  def initialize(@x : Int32, @y : Int32, @z : Int32)
  end

  def +(other : Pt3)
    Pt3.new(@x + other.x, @y + other.y, @z + other.z)
  end
end

def min(a, b)
  a < b ? a : b
end

def max(a, b)
  a > b ? a : b
end

cubes = Hash(Pt3, Bool).new
neighbors = [
  Pt3.new(-1, 0, 0),
  Pt3.new(1, 0, 0),
  Pt3.new(0, -1, 0),
  Pt3.new(0, 1, 0),
  Pt3.new(0, 0, -1),
  Pt3.new(0, 0, 1),
]

min = Pt3.new(Int32::MAX, Int32::MAX, Int32::MAX)
max = Pt3.new(Int32::MIN, Int32::MIN, Int32::MIN)

File.open("input.txt", "r") do |file|
  file.each_line do |line|
    next if line.blank?
    x, y, z = line.split(",").map(&.to_i32)
    cube = Pt3.new(x, y, z)
    cubes[cube] = true
    min = Pt3.new(min.x < x ? min.x : x, min.y < y ? min.y : y, min.z < z ? min.z : z)
    max = Pt3.new(max.x > x ? max.x : x, max.y > y ? max.y : y, max.z > z ? max.z : z)
  end
end

min = min + Pt3.new(-1, -1, -1)
max = max + Pt3.new(1, 1, 1)

faces = 0
q = [min]
seen = {min => true}

while q.any?
  curr = q.shift
  neighbors.each do |delta|
    next_cube = curr + delta
    next if next_cube.x < min.x || next_cube.y < min.y || next_cube.z < min.z || next_cube.x > max.x || next_cube.y > max.y || next_cube.z > max.z
    if cubes.has_key?(next_cube)
      faces += 1
    elsif !seen.has_key?(next_cube)
      seen[next_cube] = true
      q << next_cube
    end
  end
end

puts faces