
Point = Struct.new(:x, :y)

grid = File.readlines('input.txt').map(&:chomp)
h, w = grid.size, grid[0].size
s = nil
e = nil
track_cells = []
walls = Array.new(h) { Array.new(w, false) }

h.times do |i|
  w.times do |j|
    ch = grid[i][j]
    if ch == 'S'
      s = Point.new(i, j)
    elsif ch == 'E'
      e = Point.new(i, j)
    end
    if ch == '#'
      walls[i][j] = true
    else
      track_cells << Point.new(i, j)
    end
  end
end

dirs = [Point.new(1, 0), Point.new(-1, 0), Point.new(0, 1), Point.new(0, -1)]

normal_dist_from = lambda do |start|
  dist = Array.new(h) { Array.new(w, -1) }
  dist[start.x][start.y] = 0
  q = [start]
  until q.empty?
    cur = q.shift
    dirs.each do |d|
      nx, ny = cur.x + d.x, cur.y + d.y
      next if nx < 0 || nx >= h || ny < 0 || ny >= w
      next if walls[nx][ny]
      if dist[nx][ny] == -1
        dist[nx][ny] = dist[cur.x][cur.y] + 1
        q << Point.new(nx, ny)
      end
    end
  end
  dist
end

dist_from_s = normal_dist_from.call(s)
dist_from_e = normal_dist_from.call(e)

if dist_from_s[e.x][e.y] == -1
  puts 0
  exit
end

normal_cost = dist_from_s[e.x][e.y]

is_track = lambda do |x, y|
  return false if x < 0 || x >= h || y < 0 || y >= w
  !walls[x][y]
end

possible_cheats = 0

track_cells.each do |start_pos|
  sd = dist_from_s[start_pos.x][start_pos.y]
  next if sd == -1
  dirs.each do |d1|
    m1 = Point.new(start_pos.x + d1.x, start_pos.y + d1.y)
    next if m1.x < 0 || m1.x >= h || m1.y < 0 || m1.y >= w
    dirs.each do |d2|
      m2 = Point.new(m1.x + d2.x, m1.y + d2.y)
      next if m2.x < 0 || m2.x >= h || m2.y < 0 || m2.y >= w
      next unless is_track.call(m2.x, m2.y)
      ed = dist_from_e[m2.x][m2.y]
      next if ed == -1
      new_cost = sd + 2 + ed
      saving = normal_cost - new_cost
      possible_cheats += 1 if saving >= 100
    end
  end
end

puts possible_cheats
