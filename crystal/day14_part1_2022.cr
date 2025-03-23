
struct Point
  property x : Int32
  property y : Int32

  def initialize(@x : Int32, @y : Int32)
  end
end

def solve(input : String)
  rocks = Set(Point).new

  max_y = 0
  input.each_line do |line|
    points = line.split(" -> ").map do |coord|
      x, y = coord.split(",").map(&.to_i32)
      Point.new(x, y)
    end

    (0...points.size - 1).each do |i|
      p1 = points[i]
      p2 = points[i + 1]

      max_y = [max_y, p1.y, p2.y].max

      if p1.x == p2.x
        (p1.y < p2.y ? p1.y..p2.y : p2.y..p1.y).each do |y|
          rocks.add(Point.new(p1.x, y))
        end
      else
        (p1.x < p2.x ? p1.x..p2.x : p2.x..p1.x).each do |x|
          rocks.add(Point.new(x, p1.y))
        end
      end
    end
  end

  sand_count = 0
  loop do
    sand = Point.new(500, 0)
    loop do
      next_pos = Point.new(sand.x, sand.y + 1)
      if next_pos.y > max_y
        puts sand_count
        return
      end

      if !rocks.includes?(next_pos)
        sand = next_pos
        next
      end

      next_pos = Point.new(sand.x - 1, sand.y + 1)
      if next_pos.y > max_y
        puts sand_count
        return
      end
      if !rocks.includes?(next_pos)
        sand = next_pos
        next
      end

      next_pos = Point.new(sand.x + 1, sand.y + 1)
      if next_pos.y > max_y
        puts sand_count
        return
      end
      if !rocks.includes?(next_pos)
        sand = next_pos
        next
      end

      rocks.add(sand)
      sand_count += 1
      break
    end
  end
end

input = File.read("input.txt")
solve(input)
