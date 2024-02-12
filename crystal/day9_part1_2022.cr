
struct Point
  property x : Int32
  property y : Int32

  def initialize(@x : Int32, @y : Int32)
  end
end

file = File.open("input.txt")
head = Point.new(0, 0)
tail = Point.new(0, 0)
visited = {tail => true}

file.each_line do |line|
  dir, steps = line.split(" ")
  num_steps = steps.to_i

  num_steps.times do
    case dir
    when "R"
      head.x += 1
    when "L"
      head.x -= 1
    when "U"
      head.y += 1
    when "D"
      head.y -= 1
    end

    if (head.x - tail.x).abs > 1 || (head.y - tail.y).abs > 1
      if head.x != tail.x && head.y != tail.y
        tail.x += head.x > tail.x ? 1 : -1
        tail.y += head.y > tail.y ? 1 : -1
      else
        tail.x += head.x > tail.x ? 1 : head.x < tail.x ? -1 : 0
        tail.y += head.y > tail.y ? 1 : head.y < tail.y ? -1 : 0
      end
    end

    visited[tail] = true
  end
end

puts visited.size
