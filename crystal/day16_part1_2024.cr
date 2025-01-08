
struct State
  property x : Int32
  property y : Int32
  property d : Int32
  property cost : Int32

  def initialize(@x : Int32, @y : Int32, @d : Int32, @cost : Int32)
  end
end

struct MinHeap
  property a : Array(State)

  def initialize
    @a = [] of State
  end

  def push(v : State)
    @a << v
    up(@a.size - 1)
  end

  def pop : State
    v = @a[0]
    @a[0] = @a[-1]
    @a.pop
    down(0)
    v
  end

  private def up(i : Int32)
    while i > 0
      p = (i - 1) // 2
      break if @a[p].cost <= @a[i].cost
      @a[p], @a[i] = @a[i], @a[p]
      i = p
    end
  end

  private def down(i : Int32)
    loop do
      l = 2 * i + 1
      r = 2 * i + 2
      small = i
      small = l if l < @a.size && @a[l].cost < @a[small].cost
      small = r if r < @a.size && @a[r].cost < @a[small].cost
      break if small == i
      @a[i], @a[small] = @a[small], @a[i]
      i = small
    end
  end
end

file = File.open("input.txt")
grid = file.each_line.map(&.chomp).to_a
file.close

n = grid.size
m = grid[0].size
sx = 0
sy = 0
ex = 0
ey = 0

n.times do |i|
  m.times do |j|
    if grid[i][j] == 'S'
      sx = i
      sy = j
    elsif grid[i][j] == 'E'
      ex = i
      ey = j
    end
  end
end

dx = [-1, 0, 1, 0]
dy = [0, 1, 0, -1]

dist = Array.new(n) { Array.new(m) { Array.new(4, Int32::MAX) } }
dist[sx][sy][1] = 0

h = MinHeap.new
h.push(State.new(sx, sy, 1, 0))

while h.a.size > 0
  u = h.pop
  next if dist[u.x][u.y][u.d] < u.cost
  if u.x == ex && u.y == ey
    puts u.cost
    exit
  end

  [(u.d + 1) % 4, (u.d + 3) % 4].each do |ndir|
    nc = u.cost + 1000
    if nc < dist[u.x][u.y][ndir]
      dist[u.x][u.y][ndir] = nc
      h.push(State.new(u.x, u.y, ndir, nc))
    end
  end

  nx = u.x + dx[u.d]
  ny = u.y + dy[u.d]

  if nx >= 0 && nx < n && ny >= 0 && ny < m && grid[nx][ny] != '#'
    nc = u.cost + 1
    if nc < dist[nx][ny][u.d]
      dist[nx][ny][u.d] = nc
      h.push(State.new(nx, ny, u.d, nc))
    end
  end
end
