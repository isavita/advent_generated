class Point
  property x : Int32
  property y : Int32

  def initialize(x : Int32, y : Int32)
    @x = x
    @y = y
  end

  def hash
    @x.hash ^ @y.hash
  end

  def ==(other : Point)
    @x == other.x && @y == other.y
  end
end

class DoorMap
  @map : Hash(Point, Hash(Point, Bool))

  def initialize
    @map = Hash(Point, Hash(Point, Bool)).new
  end

  def add(p1 : Point, p2 : Point)
    @map[p1] ||= Hash(Point, Bool).new
    @map[p1][p2] = true
    @map[p2] ||= Hash(Point, Bool).new
    @map[p2][p1] = true
  end

  def [](p : Point) : Hash(Point, Bool)?
    @map[p]
  end
end

def main
  regex = File.read("input.txt").chomp
  dm = build_map(regex[1..-2])
  puts find_furthest_room(dm)
end

def build_map(regex : String) : DoorMap
  dm = DoorMap.new
  stack = [] of Point
  cp = Point.new(0, 0)

  regex.each_char do |c|
    case c
    when '('
      stack << cp
    when '|'
      cp = stack.last
    when ')'
      cp = stack.pop
    else
      np = move(cp, c)
      dm.add(cp, np)
      cp = np
    end
  end
  dm
end

def move(p : Point, dir : Char) : Point
  case dir
  when 'N' then Point.new(p.x, p.y - 1)
  when 'S' then Point.new(p.x, p.y + 1)
  when 'E' then Point.new(p.x + 1, p.y)
  when 'W' then Point.new(p.x - 1, p.y)
  else p
  end
end

def find_furthest_room(dm : DoorMap) : Int32
  visited = Hash(Point, Int32).new
  queue = [Point.new(0, 0)]
  visited[Point.new(0, 0)] = 0
  max_doors = 0

  until queue.empty?
    p = queue.shift
    dm[p].each_key do |np|
      unless visited.has_key?(np)
        visited[np] = visited[p] + 1
        max_doors = [max_doors, visited[np]].max
        queue << np
      end
    end
  end
  max_doors
end

main