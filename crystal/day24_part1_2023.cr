
struct Coord
  property x : Float64
  property y : Float64
  property z : Float64

  def initialize(@x : Float64, @y : Float64, @z : Float64)
  end
end

struct Point
  property pos : Coord
  property vel : Coord

  def initialize(@pos : Coord, @vel : Coord)
  end
end

class Solution
  def self.parse_input(input : Array(String)) : Array(Point)
    input.map do |line|
      parts = line.split(" @ ")
      pos_parts = parts[0].split(", ").map(&.to_f64)
      vel_parts = parts[1].split(", ").map(&.to_f64)
      
      Point.new(
        Coord.new(pos_parts[0], pos_parts[1], pos_parts[2]),
        Coord.new(vel_parts[0], vel_parts[1], vel_parts[2])
      )
    end
  end

  def self.is_intersecting_2d(p1 : Point, p2 : Point)
    det = p1.vel.x * p2.vel.y - p2.vel.x * p1.vel.y
    return nil if det == 0

    t1 = (p2.vel.y * (p2.pos.x - p1.pos.x) - p2.vel.x * (p2.pos.y - p1.pos.y)) / det
    t2 = (p1.vel.y * (p2.pos.x - p1.pos.x) - p1.vel.x * (p2.pos.y - p1.pos.y)) / det
    
    coord = Coord.new(
      p1.pos.x + p1.vel.x * t1,
      p1.pos.y + p1.vel.y * t1,
      0.0
    )

    {coord, t1, t2}
  end

  def self.solve(input : Array(String), min : Float64, max : Float64) : Int32
    points = parse_input(input)
    cnt = 0

    points.each_combination(2) do |(p1, p2)|
      if intersection = is_intersecting_2d(p1, p2)
        coord, time1, time2 = intersection
        if min <= coord.x <= max && 
           min <= coord.y <= max && 
           time1 >= 0 && 
           time2 >= 0
          cnt += 1
        end
      end
    end

    cnt
  end
end

def read_file(file_name : String) : Array(String)
  File.read_lines(file_name)
end

input = read_file("input.txt")
puts Solution.solve(input, 200_000_000_000_000.0, 400_000_000_000_000.0)
