
def bits(mask : UInt64) : Int32
  mask.popcount.to_i32
end

def min_weight(mat : Array(Array(Int32)), rows : Int32, cols : Int32) : Int32
  m = mat.map(&.dup)
  col_pivot = Array.new(cols, false)
  piv = 0

  (0...cols).each do |c|
    break if piv >= rows
    sel = -1
    r = piv
    while r < rows && sel == -1
      sel = r if m[r][c] == 1
      r += 1
    end
    next if sel == -1

    m[piv], m[sel] = m[sel], m[piv]

    (0...rows).each do |r2|
      next if r2 == piv || m[r2][c] != 1
      k = c
      while k <= cols
        m[r2][k] ^= m[piv][k]
        k += 1
      end
    end

    col_pivot[c] = true
    piv += 1
  end

  (piv...rows).each do |r|
    return -1 if m[r][cols] == 1
  end

  free = (0...cols).reject { |i| col_pivot[i] }
  n_free = free.size
  best = Int32::MAX
  limit = 1_u64 << n_free
  x = Array.new(cols, 0_i32)

  mask = 0_u64
  while mask < limit
    x.fill(0)
    w = bits(mask)
    j = 0
    while j < n_free
      if ((mask >> j) & 1_u64) == 1_u64
        x[free[j]] = 1
      end
      j += 1
    end

    prow = 0
    c = 0
    while c < cols
      if col_pivot[c]
        v = m[prow][cols]
        k = c + 1
        while k < cols
          v ^= (m[prow][k] == 1 ? x[k] : 0)
          k += 1
        end
        x[c] = v
        w += 1 if v == 1
        prow += 1
      end
      c += 1
    end

    best = w if w < best
    mask += 1
  end

  best
end

lines = File.read_lines("input.txt")
total = 0_i32
btn_pat = /\(([^)]*)\)/

lines.each do |raw|
  line = raw.strip
  lb = line.index('[')
  rb = line.index(']', lb || 0)
  next if lb.nil? || rb.nil?

  target_str = line[(lb + 1)...rb]
  r = target_str.size
  target = Array.new(r) { |i| target_str[i] == '#' ? 1_i32 : 0_i32 }

  btn_matches = line[(rb + 1)..-1].scan(btn_pat)
  buttons = btn_matches.map do |m|
    s = m[1].strip
    if s.empty?
      [] of Int32
    else
      s.split(',').map(&.strip.to_i)
    end
  end

  c = buttons.size
  matrix = Array.new(r) { Array.new(c + 1, 0_i32) }

  r.times do |i|
    c.times do |j|
      matrix[i][j] = buttons[j].includes?(i) ? 1 : 0
    end
    matrix[i][c] = target[i]
  end

  mw = min_weight(matrix, r.to_i32, c.to_i32)
  total += mw if mw != -1
end

puts total
