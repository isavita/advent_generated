
#!/usr/bin/env crystal

struct Point
  property x : Int32
  property y : Int32

  def initialize(@x : Int32, @y : Int32)
  end
end

DIRS = [
  Point.new(1, 0),
  Point.new(-1, 0),
  Point.new(0, 1),
  Point.new(0, -1)
]

# Read input
lines = File.read("input.txt").split('\n')
h = lines.size
w = lines.map(&.size).max

grid = lines.map { |s| s.ljust(w) }

walls = Array.new(h) { Array.new(w, false) }
s_point : Point? = nil
e_point : Point? = nil
track_cells = [] of Point

h.times do |i|
  w.times do |j|
    ch = grid[i][j]
    if ch == 'S'
      s_point = Point.new(i, j)
    elsif ch == 'E'
      e_point = Point.new(i, j)
    end
    if ch == '#'
      walls[i][j] = true
    elsif ch != ' '
      track_cells << Point.new(i, j)
    end
  end
end

s = s_point.not_nil!
e = e_point.not_nil!

def bfs(start : Point, walls : Array(Array(Bool)), h : Int32, w : Int32)
  dist = Array.new(h) { Array.new(w, -1) }
  dist[start.x][start.y] = 0
  queue = [] of Point
  queue << start
  head = 0
  while head < queue.size
    cur = queue[head]
    head += 1
    DIRS.each do |d|
      nx = cur.x + d.x
      ny = cur.y + d.y
      next if nx < 0 || nx >= h || ny < 0 || ny >= w
      next if walls[nx][ny]
      next if dist[nx][ny] != -1
      dist[nx][ny] = dist[cur.x][cur.y] + 1
      queue << Point.new(nx, ny)
    end
  end
  dist
end

dist_from_s = bfs(s, walls, h, w)
dist_from_e = bfs(e, walls, h, w)

normal_cost = dist_from_s[e.x][e.y]
if normal_cost == -1
  puts 0
  exit
end

possible_cheats = 0

track_cells.each do |start_pos|
  sd = dist_from_s[start_pos.x][start_pos.y]
  next if sd == -1
  DIRS.each do |d1|
    m1x = start_pos.x + d1.x
    m1y = start_pos.y + d1.y
    next if m1x < 0 || m1x >= h || m1y < 0 || m1y >= w
    DIRS.each do |d2|
      m2x = m1x + d2.x
      m2y = m1y + d2.y
      next if m2x < 0 || m2x >= h || m2y < 0 || m2y >= w
      next if walls[m2x][m2y]
      ed = dist_from_e[m2x][m2y]
      next if ed == -1
      new_cost = sd + 2 + ed
      possible_cheats += 1 if normal_cost - new_cost >= 100
    end
  end
end

puts possible_cheats
