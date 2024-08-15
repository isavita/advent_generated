require "file_utils"

struct Point
  getter x : Int32
  getter y : Int32

  def initialize(@x : Int32, @y : Int32)
  end
end

def get_points(path : String) : Hash(Point, Bool)
  points = Hash(Point, Bool).new
  current = Point.new(0, 0)
  path.split(',').each do |move|
    dir = move[0]
    steps = move[1..-1].to_i
    steps.times do
      case dir
      when 'U'
        current = Point.new(current.x, current.y + 1)
      when 'D'
        current = Point.new(current.x, current.y - 1)
      when 'L'
        current = Point.new(current.x - 1, current.y)
      when 'R'
        current = Point.new(current.x + 1, current.y)
      end
      points[current] = true
    end
  end
  points
end

def abs(x : Int32) : Int32
  x < 0 ? -x : x
end

data = File.read("input.txt")
lines = data.strip.split("\n")
wire1 = get_points(lines[0])
wire2 = get_points(lines[1])

intersections = Hash(Point, Bool).new
wire1.each do |point, _|
  if wire2[point]?
    intersections[point] = true
  end
end

min_distance = Int32::MAX
intersections.each do |point, _|
  distance = abs(point.x) + abs(point.y)
  min_distance = distance if distance < min_distance
end

puts min_distance