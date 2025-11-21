
struct Point
  property x : Int32
  property y : Int32
  def initialize(@x : Int32, @y : Int32); end
end

struct Move
  property steps : Int32?
  property rotate : Char?
  def initialize(@steps : Int32? = nil, @rotate : Char? = nil); end
end

N = 0
E = 1
S = 2
W = 3

DIRS = [
  Point.new(-1, 0),
  Point.new(0, 1),
  Point.new(1, 0),
  Point.new(0, -1)
]

def rotate(dir : Int32, r : Char) : Int32
  case r
  when 'R' then (dir + 1) % 4
  when 'L' then (dir - 1 + 4) % 4
  else dir
  end
end

def points(dir : Int32) : Int32
  (dir + 3) % 4
end

def parse_path(path : String) : Array(Move)
  moves = [] of Move
  acc = 0
  path.each_char do |ch|
    if ch >= '0' && ch <= '9'
      acc = acc * 10 + (ch.ord - '0'.ord)
    else
      moves << Move.new(steps: acc) if acc > 0
      acc = 0
      moves << Move.new(rotate: ch)
    end
  end
  moves << Move.new(steps: acc) if acc > 0
  moves
end

def cross_border(p : Point, dir : Int32, size : Int32) : {Point, Int32}
  x = p.x
  y = p.y
  case
  when x == -1 && y >= 0 && y < 2 * size
    {Point.new(y + 2 * size, 0), E}
  when x == -1 && y >= 2 * size
    {Point.new(4 * size - 1, y - 2 * size), N}
  when x == size && dir == S
    {Point.new(y - size, x + size - 1), W}
  when x == 2 * size - 1 && dir == N
    {Point.new(y + size, x - size + 1), E}
  when x == 3 * size && dir == S
    {Point.new(y + 2 * size, x - 2 * size - 1), W}
  when x == 4 * size
    {Point.new(p.x - 4 * size, p.y + 2 * size), S}
  when y == -1 && x >= 0 && x < 3 * size
    {Point.new(3 * size - 1 - x, y + size + 1), E}
  when y == -1 && x >= 3 * size
    {Point.new(y + 1, x - 2 * size), S}
  when y == size - 1 && x >= 0 && x < size
    {Point.new(3 * size - 1 - x, y - size + 1), E}
  when y == size - 1 && x >= size && dir == W
    {Point.new(y + size + 1, x - size), S}
  when y == size && dir == E
    {Point.new(y + 2 * size - 1, x - 2 * size), N}
  when y == 2 * size && x >= 0 && x < 2 * size && dir == E
    {Point.new(y - size - 1, x + size), N}
  when y == 2 * size && x >= 2 * size
    {Point.new(3 * size - 1 - x, y + size - 1), W}
  when y == 3 * size
    {Point.new(3 * size - 1 - x, y - size - 1), W}
  else
    raise "Logic Error: Unhandled border crossing at P{#{x}, #{y}} from dir #{dir}"
  end
end

def walk(pos : Point, facing : Int32, map : Hash({Int32, Int32}, Bool), size : Int32) : {Bool, Point, Int32}
  delta = DIRS[facing]
  nxt = Point.new(pos.x + delta.x, pos.y + delta.y)
  key = {nxt.x, nxt.y}
  case map[key]?
  when true  then {false, pos, facing}
  when false then {true, nxt, facing}
  else
    wrapped, new_dir = cross_border(nxt, facing, size)
    w_key = {wrapped.x, wrapped.y}
    case map[w_key]?
    when true  then {false, pos, facing}
    when false then {true, wrapped, new_dir}
    else
      raise "Logic Error: Cross border resulted in unmapped coordinate P{#{wrapped.x}, #{wrapped.y}}"
    end
  end
end

# ---------- main ----------
lines = File.read_lines("input.txt")
map_lines = [] of String
path_line = nil
found_blank = false

lines.each do |ln|
  if !found_blank
    if ln.empty?
      found_blank = true
    else
      map_lines << ln
    end
  elsif !ln.empty?
    path_line = ln
    break
  end
end

raise "No map found" if map_lines.empty?
size = (map_lines.first.size // 3)

map = Hash({Int32, Int32}, Bool).new
map_lines.each_with_index do |line, r_idx|
  line.each_char.with_index do |ch, c_idx|
    case ch
    when '#', '.'
      map[{r_idx, c_idx}] = (ch == '#')
    end
  end
end

raise "No path found" unless path_line
movements = parse_path(path_line)

pos = Point.new(0, size)
facing = E

movements.each do |mv|
  if mv.rotate
    facing = rotate(facing, mv.rotate.not_nil!)
  else
    steps = mv.steps.not_nil!
    steps.times do
      ok, new_pos, new_facing = walk(pos, facing, map, size)
      break unless ok
      pos = new_pos
      facing = new_facing
    end
  end
end

final_score = 1000 * (pos.x + 1) + 4 * (pos.y + 1) + points(facing)
puts final_score
