
# Crystal solution for the shortest‑path cell count problem

# ---------- Data structures ----------
struct State
  getter cost, x, y, d
  def initialize(@cost : Int32, @x : Int32, @y : Int32, @d : Int32)
  end
end

struct PointDir
  getter x, y, d
  def initialize(@x : Int32, @y : Int32, @d : Int32)
  end
end

# ---------- Priority queue ----------
class MinHeap
  @data : Array(State)

  def initialize
    @data = [] of State
  end

  def empty?
    @data.empty?
  end

  def push(state : State)
    @data << state
    idx = @data.size - 1
    while idx > 0
      parent = (idx - 1) // 2
      break if @data[parent].cost <= @data[idx].cost
      @data.swap(idx, parent)
      idx = parent
    end
  end

  def pop : State
    top = @data[0]
    last = @data.pop
    unless @data.empty?
      @data[0] = last
      idx = 0
      loop do
        left = idx * 2 + 1
        right = left + 1
        smallest = idx
        smallest = left if left < @data.size && @data[left].cost < @data[smallest].cost
        smallest = right if right < @data.size && @data[right].cost < @data[smallest].cost
        break if smallest == idx
        @data.swap(idx, smallest)
        idx = smallest
      end
    end
    top
  end
end

# ---------- Read input ----------
lines = File.read("input.txt").split('\n').map { |l| l.chomp }
n = lines.size
m = lines.empty? ? 0 : lines[0].size
grid = Array.new(n) { Array.new(m, ' ') }
sx = sy = ex = ey = -1

lines.each_with_index do |line, i|
  line.chars.each_with_index do |ch, j|
    grid[i][j] = ch
    case ch
    when 'S'
      sx = i; sy = j
    when 'E'
      ex = i; ey = j
    end
  end
end

if n == 0 || m == 0 || sx == -1 || ex == -1
  puts 0
  exit
end

INF = 1 << 30
dist = Array.new(n) { Array.new(m) { Array.new(4, INF) } }
used = Array.new(n) { Array.new(m, false) }
vis  = Array.new(n) { Array.new(m) { Array.new(4, false) } }

# ---------- Dijkstra ----------
dx = [-1, 0, 1, 0]
dy = [0, 1, 0, -1]

heap = MinHeap.new
dist[sx][sy][1] = 0
heap.push(State.new(0, sx, sy, 1))

until heap.empty?
  cur = heap.pop
  cost, x, y, d = cur.cost, cur.x, cur.y, cur.d
  next if cost > dist[x][y][d]

  # Turning
  [1, 3].each do |turn|
    nd = (d + turn) % 4
    nc = cost + 1000
    if nc < dist[x][y][nd]
      dist[x][y][nd] = nc
      heap.push(State.new(nc, x, y, nd))
    end
  end

  # Moving forward
  nx = x + dx[d]
  ny = y + dy[d]
  if nx >= 0 && nx < n && ny >= 0 && ny < m && grid[nx][ny] != '#'
    nc = cost + 1
    if nc < dist[nx][ny][d]
      dist[nx][ny][d] = nc
      heap.push(State.new(nc, nx, ny, d))
    end
  end
end

# ---------- Find best cost ----------
best = INF
4.times { |d| best = dist[ex][ey][d] if dist[ex][ey][d] < best }

if best == INF
  puts 0
  exit
end

# ---------- Backtracking ----------
rev_stack = [] of PointDir
4.times do |d|
  if dist[ex][ey][d] == best && !vis[ex][ey][d]
    vis[ex][ey][d] = true
    rev_stack << PointDir.new(ex, ey, d)
  end
end

until rev_stack.empty?
  cur = rev_stack.pop
  x, y, d = cur.x, cur.y, cur.d
  used[x][y] = true
  costU = dist[x][y][d]

  # Predecessor via turning
  [1, 3].each do |turn|
    pd = (d + turn) % 4
    if costU >= 1000 && dist[x][y][pd] == costU - 1000 && !vis[x][y][pd]
      vis[x][y][pd] = true
      rev_stack << PointDir.new(x, y, pd)
    end
  end

  # Predecessor via moving
  px = x - dx[d]
  py = y - dy[d]
  if px >= 0 && px < n && py >= 0 && py < m && grid[px][py] != '#'
    if costU > 0 && dist[px][py][d] == costU - 1 && !vis[px][py][d]
      vis[px][py][d] = true
      rev_stack << PointDir.new(px, py, d)
    end
  end
end

# ---------- Count used cells ----------
count = 0
n.times do |i|
  m.times { |j| count += 1 if used[i][j] }
end

puts count
