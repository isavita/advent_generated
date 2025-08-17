
#!/usr/bin/env crystal

struct Point
  getter y, x : Int32

  def initialize(@y : Int32, @x : Int32)
  end

  def hash
    @y.hash ^ (@x << 16)
  end

  def ==(other : Point)
    @y == other.y && @x == other.x
  end
end

# Read grid
grid = File.read_lines("input.txt")
h = grid.size
w = grid[0].bytesize

# Map each character to its antenna coordinates
antennas = Hash(Char, Array(Point)).new

grid.each_with_index do |row, y|
  row.each_char.with_index do |c, x|
    next if c == '.'
    antennas[c] ||= [] of Point
    antennas[c] << Point.new(y, x)
  end
end

# Set of antinode positions
antinodes = Set(Point).new

antennas.each_value do |coords|
  n = coords.size
  (0...n).each do |i|
    a = coords[i]
    (i + 1...n).each do |j|
      b = coords[j]
      p1 = Point.new(2 * a.y - b.y, 2 * a.x - b.x)
      p2 = Point.new(2 * b.y - a.y, 2 * b.x - a.x)

      if p1.y >= 0 && p1.y < h && p1.x >= 0 && p1.x < w
        antinodes << p1
      end
      if p2.y >= 0 && p2.y < h && p2.x >= 0 && p2.x < w
        antinodes << p2
      end
    end
  end
end

puts antinodes.size
