
Point = Struct.new(:r, :c)

def normalize(p)
  return p if p.empty?
  min_r = p.map(&:r).min
  min_c = p.map(&:c).min
  p.map { |pt| Point.new(pt.r - min_r, pt.c - min_c) }
   .sort_by { |pt| [pt.r, pt.c] }
end

def rotate(p)
  p.map { |pt| Point.new(pt.c, -pt.r) }
end

def flip(p)
  p.map { |pt| Point.new(pt.r, -pt.c) }
end

def equal?(a, b)
  a.size == b.size && a.each_with_index.all? { |pt, i| pt.r == b[i].r && pt.c == b[i].c }
end

def variations(base)
  uniq = []
  cur = base
  4.times do
    n = normalize(cur)
    uniq << n unless uniq.any? { |v| equal?(v, n) }
    f = flip(cur)
    nf = normalize(f)
    uniq << nf unless uniq.any? { |v| equal?(v, nf) }
    cur = rotate(cur)
  end
  uniq
end

def can_place?(rows, cols, grid, piece, r, c)
  piece.each do |pt|
    nr = r + pt.r
    nc = c + pt.c
    return false if nr < 0 || nr >= rows || nc < 0 || nc >= cols
    return false if grid[nr * cols + nc] != 0
  end
  true
end

def place(grid, cols, piece, r, c, v)
  piece.each { |pt| grid[(r + pt.r) * cols + (c + pt.c)] = v }
end

def check_islands(rows, cols, grid, counts, slack_idx, shapes)
  min_real = Float::INFINITY
  has_real = false
  counts.each_with_index do |cnt, i|
    next if i == slack_idx || cnt.zero?
    sz = shapes[i].size
    min_real = sz if sz < min_real
    has_real = true
  end
  return true unless has_real
  avail = counts[slack_idx]
  visited = Array.new(rows * cols, false)
  q = Array.new(rows * cols)
  (0...rows * cols).each do |start|
    next if grid[start] != 0 || visited[start]
    qs = qe = 0
    q[qe] = start; qe += 1; visited[start] = true
    size = 0
    while qs < qe
      cur = q[qs]; qs += 1; size += 1
      r = cur / cols
      c = cur % cols
      if r > 0
        n = (r - 1) * cols + c
        if grid[n] == 0 && !visited[n]; visited[n] = true; q[qe] = n; qe += 1; end
      end
      if r + 1 < rows
        n = (r + 1) * cols + c
        if grid[n] == 0 && !visited[n]; visited[n] = true; q[qe] = n; qe += 1; end
      end
      if c > 0
        n = r * cols + (c - 1)
        if grid[n] == 0 && !visited[n]; visited[n] = true; q[qe] = n; qe += 1; end
      end
      if c + 1 < cols
        n = r * cols + (c + 1)
        if grid[n] == 0 && !visited[n]; visited[n] = true; q[qe] = n; qe += 1; end
      end
    end
    if size < min_real
      return false if avail < size
      avail -= size
    end
  end
  true
end

def solve(rows, cols, grid, counts, ids, variations, slack_idx, shapes)
  empty = grid.index(0)
  return true unless empty
  r = empty / cols
  c = empty % cols
  return false unless check_islands(rows, cols, grid, counts, slack_idx, shapes)
  ids.each do |id|
    next if counts[id].zero?
    counts[id] -= 1
    variations[id].each do |var|
      if can_place?(rows, cols, grid, var, r, c)
        place(grid, cols, var, r, c, 1)
        return true if solve(rows, cols, grid, counts, ids, variations, slack_idx, shapes)
        place(grid, cols, var, r, c, 0)
      end
    end
    counts[id] += 1
  end
  false
end

lines = File.read('input.txt').lines.map { |l| l.rstrip }
max_id = -1
lines.each do |l|
  s = l.strip
  max_id = s[0..-2].to_i if s.end_with?(':') && s[0..-2].to_i > max_id
end
max_id = -1 if max_id < 0
arr_size = max_id + 2
slack_idx = max_id + 1

shapes = Array.new(arr_size) { [] }
parsing = true
cur_id = nil
cur_shape = []
region_lines = []

lines.each do |raw|
  s = raw.strip
  next if s.empty?
  if s.include?('x') && s.include?(':')
    parsing = false
  end
  if parsing
    if s.end_with?(':')
      if cur_id && !cur_shape.empty?
        pts = []
        cur_shape.each_with_index { |row, r| row.chars.each_with_index { |ch, c| pts << Point.new(r, c) if ch == '#' } }
        shapes[cur_id] = normalize(pts)
        cur_shape.clear
      end
      cur_id = s[0..-2].to_i
    else
      cur_shape << s
    end
  else
    region_lines << s
  end
end

if cur_id && !cur_shape.empty?
  pts = []
  cur_shape.each_with_index { |row, r| row.chars.each_with_index { |ch, c| pts << Point.new(r, c) if ch == '#' } }
  shapes[cur_id] = normalize(pts)
end

shapes[slack_idx] = [Point.new(0, 0)]

variations = Array.new(arr_size) { [] }
var_counts = Array.new(arr_size, 0)
(0...arr_size).each do |i|
  next if shapes[i].empty?
  vars = variations(shapes[i])
  variations[i] = vars
  var_counts[i] = vars.size
end

solved = 0
region_lines.each do |line|
  dim, cnts = line.split(':', 2)
  next unless dim && cnts
  w, h = dim.strip.split('x').map(&:to_i)
  grid_size = w * h
  piece_counts = Array.new(arr_size, 0)
  total = 0
  cnts.strip.split(/\s+/).each_with_index do |tok, idx|
    break if idx >= arr_size - 1
    c = tok.to_i
    next if c.zero?
    piece_counts[idx] = c
    total += c * shapes[idx].size
  end
  next if total > grid_size
  slack = grid_size - total
  piece_counts[slack_idx] = slack if slack > 0
  ids = (0...arr_size).select { |i| piece_counts[i] > 0 }
  ids.sort_by! { |i| -shapes[i].size }
  grid = Array.new(grid_size, 0)
  solved += 1 if solve(h, w, grid, piece_counts, ids, variations, slack_idx, shapes)
end

puts "Number of regions that fit all presents: #{solved}"
