
struct Point
  getter x : Int32
  getter y : Int32

  def initialize(@x, @y)
  end

  def +(other : self)
    Point.new(x + other.x, y + other.y)
  end

  def -(other : self)
    Point.new(x - other.x, y - other.y)
  end

  def *(scalar : Int32)
    Point.new(x * scalar, y * scalar)
  end

  def mod(rect : Rect)
    Point.new(
      (x - rect.min.x) % (rect.max.x - rect.min.x) + rect.min.x,
      (y - rect.min.y) % (rect.max.y - rect.min.y) + rect.min.y
    )
  end

  def in?(rect : Rect)
    x >= rect.min.x && x < rect.max.x && y >= rect.min.y && y < rect.max.y
  end
end

struct Rect
  getter min : Point
  getter max : Point

  def initialize(@min, @max)
  end

  def inset(n : Int32)
    Rect.new(
      Point.new(min.x + n, min.y + n),
      Point.new(max.x - n, max.y - n)
    )
  end
end

record State, pos : Point, step : Int32

NEIGHBORS4 = [
  Point.new(0, 1),
  Point.new(0, -1),
  Point.new(1, 0),
  Point.new(-1, 0)
]

DIR_FROM_BYTE = {
  '^' => Point.new(0, -1),
  '>' => Point.new(1, 0),
  'v' => Point.new(0, 1),
  '<' => Point.new(-1, 0)
}

def steps(grid, bounds, start, goal, initial_step)
  queue = Deque(State).new
  queue << State.new(start, initial_step)
  seen = Set(State).new

  until queue.empty?
    curr = queue.shift
    return curr.step if curr.pos == goal

    (NEIGHBORS4 + [Point.new(0, 0)]).each do |n|
      next_state = State.new(curr.pos + n, curr.step + 1)
      next if seen.includes?(next_state) || !next_state.pos.in?(bounds) || grid[next_state.pos]? == '#'

      if next_state.pos.y > 0 && next_state.pos.y < bounds.max.y - 1
        blizzard_found = DIR_FROM_BYTE.any? do |bliz, dir|
          prev = (next_state.pos - (dir * next_state.step)).mod(bounds.inset(1))
          grid[prev]? == bliz
        end
        next if blizzard_found
      end

      queue << next_state
      seen << next_state
    end
  end

  -1
end

input = File.read("input.txt").strip
grid = Hash(Point, Char).new
input.lines.each_with_index do |line, y|
  line.chomp.chars.each_with_index do |char, x|
    grid[Point.new(x, y)] = char if char != '.'
  end
end

bounds = Rect.new(Point.new(0, 0), Point.new(input.lines[0].chomp.size, input.lines.size))
entrance = Point.new(1, 0)
exit = Point.new(bounds.max.x - 2, bounds.max.y - 1)

crossing = steps(grid, bounds, entrance, exit, 0)
puts crossing
