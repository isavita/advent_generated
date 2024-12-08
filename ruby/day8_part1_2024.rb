
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

antinodes = {}
antennas.values.each do |coords|
  n = coords.size
  (0...n).each do |i|
    ((i + 1)...n).each do |j|
      a, b = coords[i], coords[j]
      p1 = [2 * a[0] - b[0], 2 * a[1] - b[1]]
      p2 = [2 * b[0] - a[0], 2 * b[1] - a[1]]
      antinodes[p1] = true if p1[0].between?(0, h - 1) && p1[1].between?(0, w - 1)
      antinodes[p2] = true if p2[0].between?(0, h - 1) && p2[1].between?(0, w - 1)
    end
  end
end

puts antinodes.size
