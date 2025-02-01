
grid = File.readlines("input.txt").map(&:chomp)
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
dist = Array.new(n) { Array.new(m) { [1 << 30] * 4 } }
dist[sx][sy][1] = 0
h = [[0, sx, sy, 1]]
until h.empty?
  cost, x, y, d = h.min
  h.delete([cost, x, y, d])
  next if dist[x][y][d] < cost
  break puts cost if x == ex && y == ey
  [(d + 1) % 4, (d + 3) % 4].each do |ndir|
    nc = cost + 1000
    if nc < dist[x][y][ndir]
      dist[x][y][ndir] = nc
      h << [nc, x, y, ndir]
    end
  end
  nx, ny = x + dx[d], y + dy[d]
  if nx >= 0 && nx < n && ny >= 0 && ny < m && grid[nx][ny] != '#'
    nc = cost + 1
    if nc < dist[nx][ny][d]
      dist[nx][ny][d] = nc
      h << [nc, nx, ny, d]
    end
  end
end
