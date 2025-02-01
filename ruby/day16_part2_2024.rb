
grid = File.readlines('input.txt').map(&:chomp)
n, m = grid.size, grid[0].size
sx, sy, ex, ey = 0, 0, 0, 0
grid.each_with_index do |row, i|
  row.chars.each_with_index do |cell, j|
    sx, sy = i, j if cell == 'S'
    ex, ey = i, j if cell == 'E'
  end
end

dx = [-1, 0, 1, 0]
dy = [0, 1, 0, -1]
dist = Array.new(n) { Array.new(m) { Array.new(4, Float::INFINITY) } }
dist[sx][sy][1] = 0
h = [[sx, sy, 1, 0]]

until h.empty?
  u = h.min_by { |_, _, _, cost| cost }
  h.delete(u)
  x, y, d, cost = u
  next if dist[x][y][d] < cost
  next if x == ex && y == ey

  [(d + 1) % 4, (d + 3) % 4].each do |ndir|
    nc = cost + 1000
    if nc < dist[x][y][ndir]
      dist[x][y][ndir] = nc
      h << [x, y, ndir, nc]
    end
  end
  nx, ny = x + dx[d], y + dy[d]
  if nx >= 0 && nx < n && ny >= 0 && ny < m && grid[nx][ny] != '#'
    nc = cost + 1
    if nc < dist[nx][ny][d]
      dist[nx][ny][d] = nc
      h << [nx, ny, d, nc]
    end
  end
end

best = Float::INFINITY
(0..3).each { |d| best = [best, dist[ex][ey][d]].min }
used = Array.new(n) { Array.new(m, false) }
rev = (0..3).select { |d| dist[ex][ey][d] == best }.map { |d| [ex, ey, d] }
vis = Array.new(n) { Array.new(m) { Array.new(4, false) } }
rev.each { |x, y, d| vis[x][y][d] = true }

until rev.empty?
  x, y, d = rev.pop
  used[x][y] = true
  costU = dist[x][y][d]
  [(d + 1) % 4, (d + 3) % 4].each do |pd|
    if dist[x][y][pd] == costU - 1000 && !vis[x][y][pd]
      vis[x][y][pd] = true
      rev << [x, y, pd]
    end
  end
  px, py = x - dx[d], y - dy[d]
  if px >= 0 && px < n && py >= 0 && py < m && grid[px][py] != '#'
    if dist[px][py][d] == costU - 1 && !vis[px][py][d]
      vis[px][py][d] = true
      rev << [px, py, d]
    end
  end
end

cnt = 0
used.each_with_index do |row, i|
  row.each_with_index do |cell, j|
    cnt += 1 if cell && grid[i][j] != '#'
  end
end

puts cnt
