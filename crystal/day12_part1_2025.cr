
struct Point
  getter r : Int32
  getter c : Int32
  def initialize(@r : Int32, @c : Int32); end
end

struct Piece
  getter pts : Array(Point)
  getter n : Int32
  def initialize(@pts : Array(Point))
    @n = @pts.size
  end
end

def cmp(a : Point, b : Point) : Bool
  a.r != b.r ? a.r < b.r : a.c < b.c
end

def normalize(p : Piece) : Piece
  return p if p.n == 0
  min_r = p.pts.min_by(&.r).r
  min_c = p.pts.min_by(&.c).c
  arr = p.pts.map { |pt| Point.new(pt.r - min_r, pt.c - min_c) }
  arr.sort! { |a, b| cmp(a, b) }
  Piece.new(arr)
end

def rotate(p : Piece) : Piece
  Piece.new(p.pts.map { |pt| Point.new(pt.c, -pt.r) })
end

def flip(p : Piece) : Piece
  Piece.new(p.pts.map { |pt| Point.new(pt.r, -pt.c) })
end

def equal(a : Piece, b : Piece) : Bool
  a.n == b.n && a.pts == b.pts
end

def generate_variations(base : Piece) : Array(Piece)
  uniq = [] of Piece
  cur = base
  4.times do
    n = normalize(cur)
    uniq << n unless uniq.any? { |x| equal(x, n) }
    f = flip(cur)
    nf = normalize(f)
    uniq << nf unless uniq.any? { |x| equal(x, nf) }
    cur = rotate(cur)
  end
  uniq
end

def can_place(rows, cols, grid, p, r, c) : Bool
  i = 0
  while i < p.n
    nr = r + p.pts[i].r
    nc = c + p.pts[i].c
    return false if nr < 0 || nr >= rows || nc < 0 || nc >= cols
    return false if grid[nr * cols + nc] != 0_u8
    i += 1
  end
  true
end

def place(cols, grid, p, r, c, v) : Nil
  i = 0
  while i < p.n
    grid[(r + p.pts[i].r) * cols + (c + p.pts[i].c)] = v
    i += 1
  end
end

def check_islands(rows, cols, grid, counts, arr_size, slack_idx, shapes) : Bool
  min_real = Int32::MAX
  has_real = false
  i = 0
  while i < arr_size
    if i != slack_idx && counts[i] > 0
      min_real = shapes[i].n if shapes[i].n < min_real
      has_real = true
    end
    i += 1
  end
  return true unless has_real
  avail = counts[slack_idx]
  vis = Array.new(rows * cols, false)
  q = Array.new(rows * cols, 0)
  idx = 0
  while idx < rows * cols
    if grid[idx] == 0_u8 && !vis[idx]
      qs = 0
      qe = 0
      q[qe] = idx; qe += 1; vis[idx] = true
      size = 0
      while qs < qe
        cur = q[qs]; qs += 1; size += 1
        r = cur // cols
        c = cur % cols
        if r > 0
          n = (r - 1) * cols + c
          if grid[n] == 0_u8 && !vis[n]; vis[n] = true; q[qe] = n; qe += 1; end
        end
        if r < rows - 1
          n = (r + 1) * cols + c
          if grid[n] == 0_u8 && !vis[n]; vis[n] = true; q[qe] = n; qe += 1; end
        end
        if c > 0
          n = r * cols + (c - 1)
          if grid[n] == 0_u8 && !vis[n]; vis[n] = true; q[qe] = n; qe += 1; end
        end
        if c < cols - 1
          n = r * cols + (c + 1)
          if grid[n] == 0_u8 && !vis[n]; vis[n] = true; q[qe] = n; qe += 1; end
        end
      end
      if size < min_real
        return false if avail < size
        avail -= size
      end
    end
    idx += 1
  end
  true
end

def solve_rec(rows, cols, grid, counts, arr_size, ids, id_count,
              variations, var_counts, slack_idx, shapes) : Bool
  empty = -1
  i = 0
  while i < rows * cols && empty == -1
    empty = i if grid[i] == 0_u8
    i += 1
  end
  return true if empty == -1
  r = empty // cols
  c = empty % cols
  return false unless check_islands(rows, cols, grid, counts, arr_size, slack_idx, shapes)
  ii = 0
  while ii < id_count
    id = ids[ii]
    if counts[id] > 0
      counts[id] -= 1
      v = 0
      while v < var_counts[id]
        p = variations[id][v]
        if can_place(rows, cols, grid, p, r, c)
          place(cols, grid, p, r, c, 1_u8)
          return true if solve_rec(rows, cols, grid, counts, arr_size,
                                   ids, id_count, variations, var_counts,
                                   slack_idx, shapes)
          place(cols, grid, p, r, c, 0_u8)
        end
        v += 1
      end
      counts[id] += 1
    end
    ii += 1
  end
  false
end

lines = File.read_lines("input.txt")
max_id = -1_000_000
lines.each do |l|
  s = l.strip
  if s.ends_with?(':')
    id = s[0...-1].to_i
    max_id = id if id > max_id
  end
end
max_id = -1 if max_id < 0
arr_size = max_id + 2
slack_idx = max_id + 1

shapes = Array.new(arr_size) { Piece.new([] of Point) }
parsing_shapes = true
current_id = -1
cur_shape = [] of String
region_lines = [] of String

lines.each do |raw|
  s = raw.strip
  next if s.empty?
  parsing_shapes = false if s.includes?('x') && s.includes?(':')
  if parsing_shapes
    if s.ends_with?(':')
      unless current_id == -1 || cur_shape.empty?
        pts = [] of Point
        cur_shape.each_with_index do |row, r|
          row.each_char.with_index do |ch, c|
            pts << Point.new(r, c) if ch == '#'
          end
        end
        shapes[current_id] = normalize(Piece.new(pts))
        cur_shape.clear
      end
      current_id = s[0...-1].to_i
    else
      cur_shape << s
    end
  else
    region_lines << s
  end
end

unless current_id == -1 || cur_shape.empty?
  pts = [] of Point
  cur_shape.each_with_index do |row, r|
    row.each_char.with_index do |ch, c|
      pts << Point.new(r, c) if ch == '#'
    end
  end
  shapes[current_id] = normalize(Piece.new(pts))
end

shapes[slack_idx] = Piece.new([Point.new(0, 0)])

variations = Array.new(arr_size) { [] of Piece }
var_counts = Array.new(arr_size, 0)
0.upto(arr_size - 1) do |i|
  if shapes[i].n > 0
    vars = generate_variations(shapes[i])
    variations[i] = vars
    var_counts[i] = vars.size
  end
end

solved = 0
region_lines.each do |line|
  parts = line.split(':')
  next unless parts.size == 2
  dims = parts[0].strip
  counts_str = parts[1].strip
  dim_parts = dims.split('x')
  next unless dim_parts.size == 2
  wx = dim_parts[0].to_i
  h = dim_parts[1].to_i
  grid_size = wx * h
  piece_counts = Array.new(arr_size, 0)
  total_area = 0
  toks = counts_str.split(/\s+/)
  idx = 0
  while idx < toks.size && idx < arr_size - 1
    c = toks[idx].to_i
    if c > 0
      piece_counts[idx] = c
      total_area += c * shapes[idx].n
    end
    idx += 1
  end
  next if total_area > grid_size
  slack = grid_size - total_area
  piece_counts[slack_idx] = slack if slack > 0
  ids = (0...arr_size).select { |i| piece_counts[i] > 0 }
  sorted_ids = ids.sort_by { |i| -shapes[i].n }
  grid = Array.new(grid_size, 0_u8)
  if solve_rec(h, wx, grid, piece_counts, arr_size,
               sorted_ids, sorted_ids.size,
               variations, var_counts, slack_idx, shapes)
    solved += 1
  end
end

puts "Number of regions that fit all presents: #{solved}"
