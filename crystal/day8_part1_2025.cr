
struct Point
  property x : Int32
  property y : Int32
  property z : Int32
  def initialize(@x, @y, @z); end
end

struct Edge
  property u : Int32
  property v : Int32
  property d : Int64
  def initialize(@u, @v, @d); end
end

def find(parent, x)
  while parent[x] != x
    parent[x] = parent[parent[x]]
    x = parent[x]
  end
  x
end

def union(parent, size, a, b)
  ra = find(parent, a)
  rb = find(parent, b)
  return if ra == rb
  if size[ra] < size[rb]
    ra, rb = rb, ra
  end
  parent[rb] = ra
  size[ra] += size[rb]
end

points = [] of Point
File.each_line("input.txt") do |line|
  parts = line.split(',').map(&.strip)
  next unless parts.size == 3
  x = parts[0].to_i
  y = parts[1].to_i
  z = parts[2].to_i
  points << Point.new(x, y, z)
end

n = points.size
if n < 2
  puts "Not enough points to form circuits."
  exit
end

edges = [] of Edge
(0...n).each do |i|
  ((i + 1)...n).each do |j|
    dx = points[i].x - points[j].x
    dy = points[i].y - points[j].y
    dz = points[i].z - points[j].z
    d = dx.to_i64 * dx + dy.to_i64 * dy + dz.to_i64 * dz
    edges << Edge.new(i, j, d)
  end
end

edges.sort_by!(&.d)

parent = (0...n).to_a
size = Array.new(n, 1)

limit = {edges.size, 1000}.min
(0...limit).each do |i|
  e = edges[i]
  union(parent, size, e.u, e.v)
end

top = [0, 0, 0]
(0...n).each do |i|
  if parent[i] == i
    s = size[i]
    if s > top[0]
      top[2] = top[1]
      top[1] = top[0]
      top[0] = s
    elsif s > top[1]
      top[2] = top[1]
      top[1] = s
    elsif s > top[2]
      top[2] = s
    end
  end
end

result = 1_u64
top.each { |v| result *= v if v > 0 }
puts "Product of three largest circuit sizes: #{result}"
