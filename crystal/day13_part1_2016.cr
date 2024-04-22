struct Point
  getter x : Int32
  getter y : Int32

  def initialize(@x : Int32, @y : Int32)
  end
end

def is_wall(favorite_number, x, y)
  num = x*x + 3*x + 2*x*y + y + y*y + favorite_number
  bits = 0
  while num > 0
    bits += 1 if num % 2 == 1
    num //= 2
  end
  bits % 2 != 0
end

def bfs(start, target, favorite_number)
  visited = Hash(Point, Bool).new
  queue = [start]
  steps = 0

  while queue.size > 0
    size = queue.size
    size.times do
      point = queue.shift
      if point == target
        return steps
      end

      [{1, 0}, {-1, 0}, {0, 1}, {0, -1}].each do |dx, dy|
        next_x, next_y = point.x + dx, point.y + dy
        next_point = Point.new(next_x, next_y)
        if next_x >= 0 && next_y >= 0 && !is_wall(favorite_number, next_x, next_y) && !visited[next_point]?
          visited[next_point] = true
          queue << next_point
        end
      end
    end
    steps += 1
  end
  -1
end

favorite_number = File.read("input.txt").to_i
start = Point.new(1, 1)
target = Point.new(31, 39)
steps = bfs(start, target, favorite_number)
puts steps