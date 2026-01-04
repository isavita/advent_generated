
#!/usr/bin/env crystal
# frozen_string_literal: true

struct Pt
  property x : Int32
  property y : Int32
  def initialize(@x : Int32, @y : Int32); end
end

def main
  points = [] of Pt
  uniq_x = Set(Int32).new
  uniq_y = Set(Int32).new

  File.open("input.txt", "r") do |file|
    file.each_line do |line|
      s = line.strip
      next if s.empty?
      parts = s.split(',')
      next unless parts.size == 2
      x = parts[0].to_i
      y = parts[1].to_i
      points << Pt.new(x, y)
      uniq_x << x
      uniq_y << y
    end
  end

  if points.empty?
    puts "No points found."
    return
  end

  xs = uniq_x.to_a.sort!
  ys = uniq_y.to_a.sort!
  x_idx = Hash(Int32, Int32).new
  y_idx = Hash(Int32, Int32).new
  xs.each_with_index { |v, i| x_idx[v] = i }
  ys.each_with_index { |v, i| y_idx[v] = i }

  w = 2 * xs.size + 1
  h = 2 * ys.size + 1
  col_w = Array(Int64).new(w, 0)
  row_h = Array(Int64).new(h, 0)

  col_w[0] = 1
  xs.each_with_index do |x, i|
    col_w[2 * i + 1] = 1
    if i < xs.size - 1
      gap = xs[i + 1] - xs[i] - 1
      col_w[2 * i + 2] = gap > 0 ? gap : 0
    else
      col_w[2 * i + 2] = 1
    end
  end

  row_h[0] = 1
  ys.each_with_index do |y, i|
    row_h[2 * i + 1] = 1
    if i < ys.size - 1
      gap = ys[i + 1] - ys[i] - 1
      row_h[2 * i + 2] = gap > 0 ? gap : 0
    else
      row_h[2 * i + 2] = 1
    end
  end

  grid = Array.new(h) { Array.new(w, 0_u8) }

  to_grid = ->(p : Pt) do
    gx = 2 * x_idx[p.x] + 1
    gy = 2 * y_idx[p.y] + 1
    {gx, gy}
  end

  points.each_index do |i|
    p1 = points[i]
    p2 = points[(i + 1) % points.size]
    gx1, gy1 = to_grid.call(p1)
    gx2, gy2 = to_grid.call(p2)

    if gx1 == gx2
      s, e = gy1 <= gy2 ? {gy1, gy2} : {gy2, gy1}
      (s..e).each do |y|
        grid[y][gx1] = 1_u8 if row_h[y] > 0
      end
    else
      s, e = gx1 <= gx2 ? {gx1, gx2} : {gx2, gx1}
      (s..e).each do |x|
        grid[gy1][x] = 1_u8 if col_w[x] > 0
      end
    end
  end

  # flood‑fill from (0,0)
  q = [] of Pt
  q << Pt.new(0, 0)
  grid[0][0] = 2_u8
  dirs = [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]
  idx = 0
  while idx < q.size
    cur = q[idx]
    idx += 1
    dirs.each do |dx, dy|
      nx = cur.x + dx
      ny = cur.y + dy
      next if nx < 0 || nx >= w || ny < 0 || ny >= h
      next unless grid[ny][nx] == 0_u8
      grid[ny][nx] = 2_u8
      q << Pt.new(nx, ny)
    end
  end

  # prefix sums of interior area
  pref = Array.new(h) { Array.new(w, 0_i64) }
  (0...h).each do |y|
    row_sum = 0_i64
    (0...w).each do |x|
      cell_area = grid[y][x] != 2_u8 ? col_w[x] * row_h[y] : 0_i64
      row_sum += cell_area
      above = y > 0 ? pref[y - 1][x] : 0_i64
      pref[y][x] = row_sum + above
    end
  end

  get_sum = ->(x1 : Int32, y1 : Int32, x2 : Int32, y2 : Int32) do
    lx, rx = x1 <= x2 ? {x1, x2} : {x2, x1}
    ly, ry = y1 <= y2 ? {y1, y2} : {y2, y1}
    res = pref[ry][rx]
    res -= pref[ry][lx - 1] if lx > 0
    res -= pref[ly - 1][rx] if ly > 0
    res += pref[ly - 1][lx - 1] if lx > 0 && ly > 0
    res
  end

  max_area = 0_i64
  (0...points.size).each do |i|
    ((i)...points.size).each do |j|
      p1 = points[i]
      p2 = points[j]
      area = ( (p1.x - p2.x).abs.to_i64 + 1 ) *
             ( (p1.y - p2.y).abs.to_i64 + 1 )
      next if area <= max_area
      gx1, gy1 = to_grid.call(p1)
      gx2, gy2 = to_grid.call(p2)
      max_area = area if get_sum.call(gx1, gy1, gx2, gy2) == area
    end
  end

  puts "Largest valid area: #{max_area}"
end

main
