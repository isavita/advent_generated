
struct Point
  property x : Int32
  property y : Int32
  def initialize(@x : Int32, @y : Int32); end
end

struct Blizzard
  property x : Int32
  property y : Int32
  property dir : Int32
  def initialize(@x : Int32, @y : Int32, @dir : Int32); end
end

struct State
  property x : Int32
  property y : Int32
  property t : Int32
  def initialize(@x : Int32, @y : Int32, @t : Int32); end
end

def gcd(a : Int32, b : Int32) : Int32
  while b != 0
    a, b = b, a % b
  end
  a
end

def lcm(a : Int32, b : Int32) : Int32
  return 0 if a == 0 || b == 0
  (a // gcd(a, b)) * b
end

def pos_mod(i : Int32, n : Int32) : Int32
  (i % n + n) % n
end

DIR_CHARS = {'>', '<', 'v', '^'}
DX = [0, 1, -1, 0, 0]
DY = [0, 0, 0, 1, -1]

lines = File.read_lines("input.txt")
height = lines.size
width = lines[0].size

walls = Array.new(height) { Array.new(width, false) }
blizzards = [] of Blizzard

lines.each_with_index do |line, y|
  line.each_char.with_index do |ch, x|
    if ch == '#'
      walls[y][x] = true
    elsif ch != '.'
      dir = DIR_CHARS.index(ch) || 0
      blizzards << Blizzard.new(x, y, dir)
    end
  end
end

start_x = (0...width).find { |x| !walls[0][x] }.not_nil!
end_x   = (0...width).find { |x| !walls[height - 1][x] }.not_nil!
start_pos = Point.new(start_x, 0)
end_pos   = Point.new(end_x, height - 1)

inner_w = width - 2
inner_h = height - 2
period = lcm(inner_w, inner_h)
period = 1 if period == 0

blizzard_grid = Array.new(period * height * width, false)

period.times do |t|
  blizzards.each do |b|
    nx = b.x
    ny = b.y
    case b.dir
    when 0 # >
      nx = 1 + pos_mod(b.x - 1 + t, inner_w)
    when 1 # <
      nx = 1 + pos_mod(b.x - 1 - t, inner_w)
    when 2 # v
      ny = 1 + pos_mod(b.y - 1 + t, inner_h)
    when 3 # ^
      ny = 1 + pos_mod(b.y - 1 - t, inner_h)
    end
    blizzard_grid[t * height * width + ny * width + nx] = true
  end
end

def bfs(start : Point, goal : Point, start_time : Int32,
        walls : Array(Array(Bool)), blizzard_grid : Array(Bool),
        height : Int32, width : Int32, period : Int32) : Int32
  visited = Array.new(height) { Array.new(width) { Array.new(period, false) } }
  queue = [] of State
  head = 0
  queue << State.new(start.x, start.y, start_time)
  visited[start.y][start.x][start_time % period] = true

  while head < queue.size
    cur = queue[head]
    head += 1
    nt = cur.t + 1
    nt_mod = nt % period

    5.times do |i|
      nx = cur.x + DX[i]
      ny = cur.y + DY[i]

      next if nx < 0 || nx >= width || ny < 0 || ny >= height
      next if walls[ny][nx]
      return nt if nx == goal.x && ny == goal.y
      next if blizzard_grid[nt_mod * height * width + ny * width + nx]
      next if visited[ny][nx][nt_mod]

      visited[ny][nx][nt_mod] = true
      queue << State.new(nx, ny, nt)
    end
  end

  -1
end

t1 = bfs(start_pos, end_pos, 0, walls, blizzard_grid, height, width, period)
t2 = bfs(end_pos, start_pos, t1, walls, blizzard_grid, height, width, period)
t3 = bfs(start_pos, end_pos, t2, walls, blizzard_grid, height, width, period)

puts t3
