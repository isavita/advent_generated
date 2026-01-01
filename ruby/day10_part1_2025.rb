
def min_weight(r, c, mat)
  col_pivot = Array.new(c, 0)
  pivot = 0
  (0...c).each do |col|
    break if pivot >= r
    sel = nil
    (pivot...r).each { |row| sel = row if mat[row][col] == 1; break if sel }
    next unless sel
    mat[pivot], mat[sel] = mat[sel], mat[pivot]
    (0...r).each do |row|
      next if row == pivot
      next unless mat[row][col] == 1
      (col..c).each { |k| mat[row][k] ^= mat[pivot][k] }
    end
    col_pivot[col] = 1
    pivot += 1
  end
  (pivot...r).each { |row| return -1 if mat[row][c] == 1 }
  free = (0...c).reject { |col| col_pivot[col] == 1 }
  n = free.size
  best = Float::INFINITY
  limit = 1 << n
  (0...limit).each do |mask|
    x = Array.new(c, 0)
    cw = mask.respond_to?(:popcount) ? mask.popcount : mask.to_s(2).count('1')
    n.times { |j| x[free[j]] = 1 if (mask >> j) & 1 == 1 }
    cur = 0
    (0...c).each do |col|
      next unless col_pivot[col] == 1
      val = mat[cur][c]
      (col + 1...c).each { |k| val ^= x[k] if mat[cur][k] == 1 }
      x[col] = val
      cw += val
      cur += 1
    end
    best = cw if cw < best
  end
  best
end

total = 0
File.readlines('input.txt', chomp: true).each do |line|
  m = line.match(/\[(.*?)\]/)
  next unless m
  target = m[1].chars.map { |ch| ch == '#' ? 1 : 0 }
  r = target.size
  buttons = line.scan(/\(([^)]*)\)/).map do |inside|
    s = inside[0]
    s.strip.empty? ? [] : s.split(',').map(&:to_i)
  end
  c = buttons.size
  mat = Array.new(r) { Array.new(c + 1, 0) }
  r.times do |row|
    c.times { |col| mat[row][col] = 1 if buttons[col].include?(row) }
    mat[row][c] = target[row]
  end
  mw = min_weight(r, c, mat)
  total += mw if mw != -1
end
puts total
