require "file_utils"

struct Point
  getter x : Int32
  getter y : Int32

  def initialize(@x, @y)
  end

  def hash
    x.hash ^ y.hash
  end

  def ==(other : Point)
    x == other.x && y == other.y
  end
end

def get_points_with_steps(path)
  points = {} of Point => Int32
  current = Point.new(0, 0)
  steps = 0
  path.split(',').each do |move|
    dir = move[0]
    dist = move[1..-1].to_i
    dist.times do
      steps += 1
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
      points[current] ||= steps
    end
  end
  points
end

lines = File.read("input.txt").split("\n")
wire1 = get_points_with_steps(lines[0])
wire2 = get_points_with_steps(lines[1])

min_steps = Int32::MAX
wire1.each do |point, steps1|
  if steps2 = wire2[point]?
    total_steps = steps1 + steps2
    min_steps = total_steps if total_steps < min_steps
  end
end

puts min_steps