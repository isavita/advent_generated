file = File.open("input.txt")
grid = Hash(Point, Int32).new

file.each_line do |line|
  line =~ /(.*) -> (.*)/
  start_coords = $1.split(',').map(&.to_i)
  end_coords = $2.split(',').map(&.to_i)

  x1, y1 = start_coords
  x2, y2 = end_coords

  if x1 == x2
    y1, y2 = y2, y1 if y1 > y2
    (y1..y2).each do |y|
      grid[Point.new(x1, y)] ||= 0
      grid[Point.new(x1, y)] += 1
    end
  elsif y1 == y2
    x1, x2 = x2, x1 if x1 > x2
    (x1..x2).each do |x|
      grid[Point.new(x, y1)] ||= 0
      grid[Point.new(x, y1)] += 1
    end
  end
end

overlap_count = 0
grid.each do |_point, count|
  overlap_count += 1 if count > 1
end

puts overlap_count

struct Point
  getter x : Int32
  getter y : Int32

  def initialize(@x, @y)
  end
end