
#!/usr/bin/env ruby
def find(p, x)
  while p[x] != x
    p[x] = p[p[x]]
    x = p[x]
  end
  x
end

def union(p, sz, a, b)
  ra = find(p, a)
  rb = find(p, b)
  return if ra == rb
  if sz[ra] < sz[rb]
    ra, rb = rb, ra
  end
  p[rb] = ra
  sz[ra] += sz[rb]
end

if __FILE__ == $0
  pts = []
  File.foreach('input.txt') do |line|
    if m = line.match(/(-?\d+)\s*,\s*(-?\d+)\s*,\s*(-?\d+)/)
      pts << [m[1].to_i, m[2].to_i, m[3].to_i]
    end
  end

  n = pts.size
  if n < 2
    puts 'Not enough points to form circuits.'
    exit
  end

  edges = []
  (0...n).each do |i|
    (i + 1...n).each do |j|
      dx = pts[i][0] - pts[j][0]
      dy = pts[i][1] - pts[j][1]
      dz = pts[i][2] - pts[j][2]
      edges << [dx * dx + dy * dy + dz * dz, i, j]
    end
  end

  edges.sort_by! { |e| e[0] }
  limit = [edges.size, 1_000].min

  parent = (0...n).to_a
  size   = Array.new(n, 1)

  (0...limit).each do |k|
    _, a, b = edges[k]
    union(parent, size, a, b)
  end

  top = [0, 0, 0]
  (0...n).each do |i|
    if parent[i] == i
      s = size[i]
      if s > top[0]
        top[2] = top[1]; top[1] = top[0]; top[0] = s
      elsif s > top[1]
        top[2] = top[1]; top[1] = s
      elsif s > top[2]
        top[2] = s
      end
    end
  end

  result = top.reduce(1) { |prod, v| prod * v }
  puts "Product of three largest circuit sizes: #{result}"
end
