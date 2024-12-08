
require 'set'

def gcd(a, b)
  b.zero? ? a.abs : gcd(b, a % b)
end

grid = File.readlines('input.txt').map(&:chomp)
h = grid.size
w = grid[0].size

antennas = {}
grid.each_with_index do |row, y|
  row.chars.each_with_index do |c, x|
    next if c == '.'
    antennas[c] ||= []
    antennas[c] << [y, x]
  end
end

lines_per_freq = {}
antennas.each do |f, coords|
  lines_per_freq[f] = Set.new
  n = coords.size
  (0...n).each do |i|
    ((i + 1)...n).each do |j|
      a = coords[i]
      b = coords[j]
      dy = b[0] - a[0]
      dx = b[1] - a[1]
      g = gcd(dy, dx)
      sy = dy / g
      sx = dx / g
      sx, sy = -sx, -sy if sx < 0 || (sx.zero? && sy < 0)
      c = sy * a[1] - sx * a[0]
      lines_per_freq[f] << "#{sx},#{sy},#{c}"
    end
  end
end

antinodes = Set.new
lines_per_freq.each do |_, lines|
  lines.each do |key|
    sx, sy, c = key.split(',').map(&:to_i)
    next if sx.zero? && sy.zero?
    if sy.zero?
      next unless c % sx == 0
      y = -c / sx
      next unless y.between?(0, h - 1)
      (0...w).each { |x| antinodes << [y, x] }
    elsif sx.zero?
      next unless c % sy == 0
      x = c / sy
      next unless x.between?(0, w - 1)
      (0...h).each { |y| antinodes << [y, x] }
    else
      (0...h).each do |y|
        val = c + sx * y
        next unless val % sy == 0
        x = val / sy
        antinodes << [y, x] if x.between?(0, w - 1)
      end
    end
  end
end

puts antinodes.size
