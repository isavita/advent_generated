#!/usr/bin/env ruby
points = []
uniq_x = {}
uniq_y = {}
File.foreach('input.txt') do |line|
  line.strip!
  next if line.empty?
  a, b = line.split(',')
  next unless b
  x = a.to_i
  y = b.to_i
  points << [x, y]
  uniq_x[x] = true
  uniq_y[y] = true
end
if points.empty?
  puts 'No points found.'
  exit
end
xs = uniq_x.keys.sort
ys = uniq_y.keys.sort
x_idx = {}
xs.each_with_index { |v, i| x_idx[v] = i }
y_idx = {}
ys.each_with_index { |v, i| y_idx[v] = i }
w = 2 * xs.size + 1
h = 2 * ys.size + 1
col_w = Array.new(w, 0)
row_h = Array.new(h, 0)
col_w[0] = 1
xs.each_with_index do |v, i|
  col_w[2 * i + 1] = 1
  if i < xs.size - 1
    gap = xs[i + 1] - v - 1
    col_w[2 * i + 2] = gap > 0 ? gap : 0
  else
    col_w[2 * i + 2] = 1
  end
end
row_h[0] = 1
ys.each_with_index do |v, i|
  row_h[2 * i + 1] = 1
  if i < ys.size - 1
    gap = ys[i + 1] - v - 1
    row_h[2 * i + 2] = gap > 0 ? gap : 0
  else
    row_h[2 * i + 2] = 1
  end
end
grid = Array.new(h) { Array.new(w, 0) }
to_grid = lambda do |p|
  gx = 2 * x_idx[p[0]] + 1
  gy = 2 * y_idx[p[1]] + 1
  [gx, gy]
end
n = points.size
n.times do |i|
  p1 = points[i]
  p2 = points[(i + 1) % n]
  gx1, gy1 = to_grid.call(p1)
  gx2, gy2 = to_grid.call(p2)
  if gx1 == gx2
    s, e = gy1 <= gy2 ? [gy1, gy2] : [gy2, gy1]
    (s..e).each { |y| grid[y][gx1] = 1 if row_h[y] > 0 }
  else
    s, e = gx1 <= gx2 ? [gx1, gx2] : [gx2, gx1]
    (s..e).each { |x| grid[gy1][x] = 1 if col_w[x] > 0 }
  end
end
queue = [[0, 0]]
grid[0][0] = 2
dirs = [[0, 1], [0, -1], [1, 0], [-1, 0]]
until queue.empty?
  x, y = queue.shift
  dirs.each do |dx, dy|
    nx = x + dx
    ny = y + dy
    next if nx < 0 || nx >= w || ny < 0 || ny >= h
    next unless grid[ny][nx] == 0
    grid[ny][nx] = 2
    queue << [nx, ny]
  end
end
pref = Array.new(h) { Array.new(w, 0) }
h.times do |y|
  w.times do |x|
    val = grid[y][x] == 2 ? 0 : col_w[x] * row_h[y]
    left = x > 0 ? pref[y][x - 1] : 0
    up = y > 0 ? pref[y - 1][x] : 0
    diag = (x > 0 && y > 0) ? pref[y - 1][x - 1] : 0
    pref[y][x] = val + left + up - diag
  end
end
sum = lambda do |x1, y1, x2, y2|
  x1, x2 = x2, x1 if x1 > x2
  y1, y2 = y2, y1 if y1 > y2
  total = pref[y2][x2]
  left = x1 > 0 ? pref[y2][x1 - 1] : 0
  up = y1 > 0 ? pref[y1 - 1][x2] : 0
  diag = (x1 > 0 && y1 > 0) ? pref[y1 - 1][x1 - 1] : 0
  total - left - up + diag
end
max_area = 0
n.times do |i|
  (i...n).each do |j|
    p1 = points[i]
    p2 = points[j]
    real_w = (p1[0] - p2[0]).abs + 1
    real_h = (p1[1] - p2[1]).abs + 1
    area = real_w * real_h
    next if area <= max_area
    gx1, gy1 = to_grid.call(p1)
    gx2, gy2 = to_grid.call(p2)
    max_area = area if sum.call(gx1, gy1, gx2, gy2) == area
  end
end
puts "Largest valid area: #{max_area}"