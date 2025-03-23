
struct Point
  property x : Int32
  property y : Int32

  def initialize(@x : Int32, @y : Int32)
  end

  def +(other : Point) : Point
    Point.new(x + other.x, y + other.y)
  end

  def to_s : String
    "(#{x}, #{y})"
  end
end

def build_map(directions : String)
  stack = [] of Tuple(Point, Hash(Point, Int32))
  rooms = Hash(Point, Int32).new
  current_room = Point.new(0, 0)
  doors_passed = Hash(Point, Int32).new
  doors_passed[current_room] = 0

  directions[1..-2].each_char do |char|
    case char
    when '('
      stack << {current_room, doors_passed.dup}
    when '|'
      current_room, doors_passed = stack.last
    when ')'
      current_room, doors_passed = stack.pop
    else
      dx, dy = case char
              when 'N' then {0, -1}
              when 'E' then {1, 0}
              when 'S' then {0, 1}
              when 'W' then {-1, 0}
              else {0, 0} # Handle unexpected characters
              end
      delta = Point.new(dx, dy)
      new_room = current_room + delta
      new_doors = doors_passed[current_room] + 1
      
      if rooms.has_key?(new_room)
        rooms[new_room] = new_doors if new_doors < rooms[new_room]
      else
        rooms[new_room] = new_doors
      end
      
      doors_passed[new_room] = new_doors
      current_room = new_room
    end
  end
  rooms
end

directions = File.read("input.txt").strip

rooms = build_map(directions)
max_doors = rooms.values.max
rooms_with_1000_doors = rooms.values.count { |doors| doors >= 1000 }

puts max_doors
puts rooms_with_1000_doors
