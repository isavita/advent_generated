#!/usr/bin/env crystal

def gcd(a : Int64, b : Int64) : Int64
  a = a.abs
  b = b.abs
  while b != 0
    a, b = b, a % b
  end
  a
end

begin
  lines = File.read_lines("input.txt")
rescue
  puts 0
  exit
end

grid = [] of String
lines.each do |line|
  next if line.empty?
  grid << line
end

h = grid.size
w = h > 0 ? grid[0].size : 0
if h == 0 || w == 0
  puts 0
  exit
end

antenna_coords = Array.new(256) { [] of Array(Int32) }

h.times do |y|
  row = grid[y]
  w.times do |x|
    ch = row[x]
    next if ch == '.'
    freq_idx = ch.ord
    antenna_coords[freq_idx] << [y, x]
  end
end

is_antinode = Array.new(h) { Array.new(w, false) }
antinode_count = 0_i64

256.times do |f|
  coords = antenna_coords[f]
  n = coords.size
  next if n < 2
  n.times do |i|
    (i + 1...n).each do |j|
      a_y, a_x = coords[i]
      b_y, b_x = coords[j]
      dy = (b_y - a_y).to_i64
      dx = (b_x - a_x).to_i64
      common = gcd(dy, dx)
      sy = dy / common
      sx = dx / common
      if sx < 0 || (sx == 0 && sy < 0)
        sx = -sx
        sy = -sy
      end
      c = sy * a_x - sx * a_y
      if sy == 0
        next if sx == 0
        next unless c % sx == 0
        y_line = -c / sx
        next unless y_line >= 0 && y_line < h
        iy = y_line.to_i32
        w.times do |ix|
          unless is_antinode[iy][ix]
            is_antinode[iy][ix] = true
            antinode_count += 1
          end
        end
      elsif sx == 0
        next unless c % sy == 0
        x_line = c / sy
        next unless x_line >= 0 && x_line < w
        ix = x_line.to_i32
        h.times do |iy|
          unless is_antinode[iy][ix]
            is_antinode[iy][ix] = true
            antinode_count += 1
          end
        end
      else
        h.times do |iy|
          num = c + sx * iy
          next unless num % sy == 0
          x_line = num / sy
          next unless x_line >= 0 && x_line < w
          ix = x_line.to_i32
          unless is_antinode[iy][ix]
            is_antinode[iy][ix] = true
            antinode_count += 1
          end
        end
      end
    end
  end
end

puts antinode_count
