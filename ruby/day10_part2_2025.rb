#!/usr/bin/env ruby
def solve(buttons, targets)
  n = targets.size
  b = buttons.size
  m = Array.new(n) { Array.new(b + 1, 0.0) }
  n.times { |i| m[i][b] = targets[i] }
  b.times do |i|
    buttons[i].each { |idx| m[idx][i] = 1.0 if idx < n }
  end
  piv = Array.new(n, -1)
  r = 0
  b.times do |c|
    break if r >= n
    mr = r
    (r...n).each { |i| mr = i if m[i][c].abs > m[mr][c].abs }
    next if m[mr][c].abs < 1e-9
    m[r], m[mr] = m[mr], m[r]
    s = m[r][c]
    (c..b).each { |k| m[r][k] /= s }
    n.times do |i|
      next if i == r || m[i][c].abs < 1e-9
      f = m[i][c]
      (c..b).each { |k| m[i][k] -= f * m[r][k] }
    end
    piv[r] = c
    r += 1
  end
  rank = r
  (rank...n).each { |i| return -1 if m[i][b].abs > 1e-9 }
  is_p = Array.new(b, false)
  piv.each { |c| is_p[c] = true if c >= 0 }
  free = (0...b).reject { |i| is_p[i] }
  max = Array.new(b) do |i|
    mn = Float::INFINITY
    buttons[i].each { |j| mn = [mn, targets[j]].min if j < n }
    mn == Float::INFINITY ? 0 : mn.to_i
  end
  free.sort_by! { |i| max[i] }
  best = Float::INFINITY
  fv = Array.new(free.size, 0)
  enum = lambda do |idx, sum|
    return if sum >= best
    if idx == free.size
      res = Array.new(b, 0)
      free.each_with_index { |f, i| res[f] = fv[i] }
      (rank - 1).downto(0) do |i|
        c = piv[i]
        next if c < 0
        v = m[i][b]
        (c + 1...b).each { |k| v -= m[i][k] * res[k] }
        iv = v.round
        return if (v - iv).abs > 1e-6 || iv < 0 || iv > max[c]
        res[c] = iv
      end
      cur = res.sum
      best = cur if cur < best
    else
      f = free[idx]
      (0..max[f]).each do |val|
        fv[idx] = val
        enum.call(idx + 1, sum + val)
      end
    end
  end
  enum.call(0, 0)
  best == Float::INFINITY ? -1 : best
end

total = 0
File.foreach('input.txt') do |line|
  line = line.strip
  next if line.empty?
  buttons = []
  line.scan(/\(([^)]*)\)/) do |m|
    s = m[0].strip
    if s.empty?
      buttons << []
    else
      buttons << s.split(/,/).map { |x| x.strip.to_i }
    end
  end
  target_str = line[/\{([^}]*)\}/, 1]
  next unless target_str
  targets = target_str.split(/,/).map { |x| x.strip.to_i }
  total += solve(buttons, targets)
end
puts total