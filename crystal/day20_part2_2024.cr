
MAX_STEPS = 20
INF = -1

struct Point
  property r : Int32
  property c : Int32
  def initialize(@r : Int32, @c : Int32); end
end

def bfs(start : Point, walls, h, w)
  dist = Array.new(h) { Array.new(w, INF) }
  return dist unless start.r >= 0 && start.r < h && start.c >= 0 && start.c < w && walls[start.r][start.c] == 0
  q = Array(Point).new(h * w)
  head = 0
  tail = 0
  dist[start.r][start.c] = 0
  q << start
  tail += 1
  dr = [1, -1, 0, 0]
  dc = [0, 0, 1, -1]
  while head < tail
    cur = q[head]
    head += 1
    d = dist[cur.r][cur.c]
    4.times do |i|
      nr = cur.r + dr[i]
      nc = cur.c + dc[i]
      if nr >= 0 && nr < h && nc >= 0 && nc < w && walls[nr][nc] == 0 && dist[nr][nc] == INF
        dist[nr][nc] = d + 1
        q << Point.new(nr, nc)
        tail += 1
      end
    end
  end
  dist
end

def limited_bfs(start : Point, walls, h, w)
  dist = Array.new(h) { Array.new(w, INF) }
  return dist unless start.r >= 0 && start.r < h && start.c >= 0 && start.c < w
  q = Array(Point).new(h * w)
  head = 0
  tail = 0
  dist[start.r][start.c] = 0
  q << start
  tail += 1
  dr = [1, -1, 0, 0]
  dc = [0, 0, 1, -1]
  while head < tail
    cur = q[head]
    head += 1
    d = dist[cur.r][cur.c]
    next if d >= MAX_STEPS
    4.times do |i|
      nr = cur.r + dr[i]
      nc = cur.c + dc[i]
      if nr >= 0 && nr < h && nc >= 0 && nc < w && dist[nr][nc] == INF
        dist[nr][nc] = d + 1
        q << Point.new(nr, nc)
        tail += 1
      end
    end
  end
  dist
end

grid = [] of String
walls = [] of Array(Int32)
s = Point.new(-1, -1)
e = Point.new(-1, -1)

File.each_line("input.txt") do |line|
  row = line.chomp
  grid << row
end

h = grid.size
w = grid[0].size
h.times do |i|
  walls << Array.new(w, 0)
end

h.times do |i|
  w.times do |j|
    ch = grid[i][j]
    if ch == 'S'
      s = Point.new(i, j)
    elsif ch == 'E'
      e = Point.new(i, j)
    end
    walls[i][j] = ch == '#' ? 1 : 0
  end
end

dist_s = bfs(s, walls, h, w)
dist_e = bfs(e, walls, h, w)
normal_cost = dist_s[e.r][e.c]

if normal_cost == INF
  puts 0
  exit
end

cheat_count = 0_i64

h.times do |sr|
  w.times do |sc|
    next if walls[sr][sc] == 1 || dist_s[sr][sc] == INF
    sd = dist_s[sr][sc]
    start_cheat = Point.new(sr, sc)
    dist_c = limited_bfs(start_cheat, walls, h, w)
    h.times do |er|
      w.times do |ec|
        next if walls[er][ec] == 1 || dist_e[er][ec] == INF
        s_steps = dist_c[er][ec]
        if s_steps > 0 && s_steps <= MAX_STEPS
          total = sd + s_steps + dist_e[er][ec]
          if total < normal_cost && (normal_cost - total) >= 100
            cheat_count += 1
          end
        end
      end
    end
  end
end

puts cheat_count
