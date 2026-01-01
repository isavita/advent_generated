
def find(p, x)
  while p[x] != x
    p[x] = p[p[x]]
    x = p[x]
  end
  x
end

def unite(p, r, a, b)
  a = find(p, a)
  b = find(p, b)
  return if a == b
  if r[a] < r[b]
    p[a] = b
  elsif r[a] > r[b]
    p[b] = a
  else
    p[b] = a
    r[a] += 1
  end
end

pts = []
File.foreach('input.txt') do |line|
  line = line.strip
  next if line.empty?
  a, b, c = line.split(',').map(&:to_i)
  pts << [a, b, c]
end

n = pts.size
exit if n < 2

edges = []
(0...n).each do |i|
  ((i + 1)...n).each do |j|
    dx = pts[i][0] - pts[j][0]
    dy = pts[i][1] - pts[j][1]
    dz = pts[i][2] - pts[j][2]
    d = dx * dx + dy * dy + dz * dz
    edges << [d, i, j]
  end
end
edges.sort_by! { |e| e[0] }

parent = (0...n).to_a
rank   = Array.new(n, 0)
comps  = n

edges.each do |_, u, v|
  ru = find(parent, u)
  rv = find(parent, v)
  next if ru == rv
  unite(parent, rank, ru, rv)
  comps -= 1
  if comps == 1
    p1 = pts[u]
    p2 = pts[v]
    puts "Connected #{p1[0]},#{p1[1]},#{p1[2]} and #{p2[0]},#{p2[1]},#{p2[2]}"
    puts "Product of X coordinates: #{p1[0].to_i * p2[0].to_i}"
    break
  end
end
