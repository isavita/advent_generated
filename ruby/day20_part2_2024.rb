
grid = File.readlines('input.txt').map(&:chomp)
h, w = grid.length, grid[0].length

s = nil
e = nil
walls = Array.new(h) { Array.new(w, false) }
track_cells = []

h.times do |i|
  w.times do |j|
    ch = grid[i][j]
    if ch == 'S'
      s = [i, j]
    elsif ch == 'E'
      e = [i, j]
    end
    walls[i][j] = true if ch == '#'
    track_cells << [i, j] unless ch == '#'
  end
end

dirs = [[1, 0], [-1, 0], [0, 1], [0, -1]]

is_track = lambda do |x, y|
  x >= 0 && x < h && y >= 0 && y < w && !walls[x][y]
end

normal_dist_from = lambda do |start|
  dist = Array.new(h) { Array.new(w, -1) }
  dist[start[0]][start[1]] = 0
  q = [start]
  until q.empty?
    cur = q.shift
    dirs.each do |d|
      nx, ny = cur[0] + d[0], cur[1] + d[1]
      next if nx < 0 || nx >= h || ny < 0 || ny >= w
      next if walls[nx][ny]
      if dist[nx][ny] < 0
        dist[nx][ny] = dist[cur[0]][cur[1]] + 1
        q << [nx, ny]
      end
    end
  end
  dist
end

dist_from_s = normal_dist_from.call(s)
dist_from_e = normal_dist_from.call(e)

if dist_from_s[e[0]][e[1]] < 0
  puts 0
  exit
end

normal_cost = dist_from_s[e[0]][e[1]]
cheats = {}

track_cells.each do |start_pos|
  sd = dist_from_s[start_pos[0]][start_pos[1]]
  next if sd < 0

  dist_c = Array.new(h) { Array.new(w, -1) }
  dist_c[start_pos[0]][start_pos[1]] = 0
  q = [start_pos]

  until q.empty?
    cur = q.shift
    steps = dist_c[cur[0]][cur[1]]
    break if steps == 20

    dirs.each do |d|
      nx, ny = cur[0] + d[0], cur[1] + d[1]
      next if nx < 0 || nx >= h || ny < 0 || ny >= w

      if dist_c[nx][ny] < 0
        dist_c[nx][ny] = steps + 1
        q << [nx, ny]
      end
    end
  end

  h.times do |x|
    w.times do |y|
      ss = dist_c[x][y]
      next unless ss > 0 && ss <= 20 && is_track.call(x, y)

      ed = dist_from_e[x][y]
      next if ed < 0

      cost = sd + ss + ed
      if cost < normal_cost
        key = [start_pos[0], start_pos[1], x, y]
        cheats[key] = cost if !cheats.key?(key) || cost < cheats[key]
      end
    end
  end
end

count = cheats.values.count { |cost| normal_cost - cost >= 100 }
puts count
