
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

def union(parent, rank, a, b)
  if rank[a] < rank[b]
    parent[a] = b
  elsif rank[a] > rank[b]
    parent[b] = a
  else
    parent[b] = a
    rank[a] += 1
  end
end

def dist_sq(a, b)
  dx = a.x - b.x
  dy = a.y - b.y
  dz = a.z - b.z
  dx.to_i64*dx + dy.to_i64*dy + dz.to_i64*dz
end

points = [] of Point
File.each_line("input.txt") do |line|
  line = line.strip
  next if line.empty?
  parts = line.split(',')
  next unless parts.size == 3
  begin
    x = parts[0].to_i
    y = parts[1].to_i
    z = parts[2].to_i
    points << Point.new(x, y, z)
  rescue
  end
end

exit if points.size < 2

n = points.size
edges = [] of Edge
(0...n).each do |i|
  ((i + 1)...n).each do |j|
    edges << Edge.new(i, j, dist_sq(points[i], points[j]))
  end
end

edges.sort_by!(&.d)

parent = (0...n).to_a
rank   = Array.new(n, 0)
comps  = n

edges.each do |e|
  break if comps == 1
  ru = find(parent, e.u)
  rv = find(parent, e.v)
  next if ru == rv
  union(parent, rank, ru, rv)
  comps -= 1
  if comps == 1
    p1 = points[e.u]
    p2 = points[e.v]
    puts "Connected #{p1.x},#{p1.y},#{p1.z} and #{p2.x},#{p2.y},#{p2.z}"
    puts "Product of X coordinates: #{p1.x.to_i64 * p2.x}"
  end
end
