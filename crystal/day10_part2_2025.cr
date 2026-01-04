
INF = 0x3f3f3f3f

def parse_line(line)
  buttons = [] of Array(Int32)
  targets = [] of Int32
  i = 0
  while i < line.size
    case line[i]
    when '('
      i += 1
      btn = [] of Int32
      while i < line.size && line[i] != ')'
        num = 0
        while i < line.size && line[i] >= '0' && line[i] <= '9'
          num = num * 10 + (line[i] - '0')
          i += 1
        end
        btn << num
        i += 1 if i < line.size && line[i] == ','
      end
      i += 1 if i < line.size && line[i] == ')'
      buttons << btn
    when '{'
      i += 1
      while i < line.size && line[i] != '}'
        num = 0
        while i < line.size && line[i] >= '0' && line[i] <= '9'
          num = num * 10 + (line[i] - '0')
          i += 1
        end
        targets << num
        i += 1 if i < line.size && line[i] == ','
      end
      break
    else
      i += 1
    end
  end
  {buttons, targets}
end

def gauss(buttons, targets)
  n_c = targets.size
  n_b = buttons.size
  matrix = Array.new(n_c) { Array.new(n_b + 1, 0.0) }
  n_c.times { |r| matrix[r][n_b] = targets[r].to_f }
  n_b.times do |b|
    buttons[b].each do |c|
      matrix[c][b] = 1.0 if c < n_c
    end
  end

  pivot_col = Array.new(n_c, -1)
  row = 0
  (0...n_b).each do |col|
    break if row >= n_c
    max_row = row
    (row...n_c).each do |r|
      max_row = r if matrix[r][col].abs > matrix[max_row][col].abs
    end
    next if matrix[max_row][col].abs < 1e-9
    matrix[row], matrix[max_row] = matrix[max_row], matrix[row]
    scale = matrix[row][col]
    (col..n_b).each { |c| matrix[row][c] /= scale }
    n_c.times do |r|
      next if r == row || matrix[r][col].abs < 1e-9
      factor = matrix[r][col]
      (col..n_b).each { |c| matrix[r][c] -= factor * matrix[row][c] }
    end
    pivot_col[row] = col
    row += 1
  end

  rank = row
  is_pivot = Array.new(n_b, false)
  pivot_rows = Array.new(n_b, -1)
  rank.times do |r|
    c = pivot_col[r]
    if c >= 0
      is_pivot[c] = true
      pivot_rows[c] = r
    end
  end
  free_vars = [] of Int32
  n_b.times { |i| free_vars << i unless is_pivot[i] }

  max_presses = Array.new(n_b, INF)
  n_b.times do |i|
    buttons[i].each do |c|
      if c < n_c && targets[c] < max_presses[i]
        max_presses[i] = targets[c]
      end
    end
    max_presses[i] = 0 if max_presses[i] == INF
  end
  free_vars.sort_by! { |v| max_presses[v] }

  {matrix, pivot_col, free_vars, max_presses, n_b, n_c}
end

def compute_pivots(matrix, pivot_col, free_vars, max_presses, presses, free_vals)
  n_b = matrix[0].size - 1
  n_c = matrix.size
  presses.fill(0)
  free_vars.each_with_index { |v, i| presses[v] = free_vals[i] }
  (n_c - 1).downto(0) do |r|
    col = pivot_col[r]
    next if col < 0
    val = matrix[r][n_b]
    (col + 1...n_b).each { |c| val -= matrix[r][c] * presses[c] }
    int_val = val.round.to_i
    return 0 if (val - int_val).abs > 1e-6
    return 0 if int_val < 0 || int_val > max_presses[col]
    presses[col] = int_val
  end
  presses.sum
end

def enumerate(idx, cur_sum, best, free_vars, max_presses, matrix, pivot_col, presses, free_vals)
  return best if cur_sum >= best
  if idx == free_vars.size
    sum = compute_pivots(matrix, pivot_col, free_vars, max_presses, presses, free_vals)
    return sum if sum > 0 && sum < best
    return best
  end
  fv = free_vars[idx]
  (0..max_presses[fv]).each do |v|
    free_vals[idx] = v
    best = enumerate(idx + 1, cur_sum + v, best, free_vars, max_presses,
                     matrix, pivot_col, presses, free_vals)
  end
  best
end

def solve(buttons, targets)
  matrix, pivot_col, free_vars, max_presses, n_b, n_c = gauss(buttons, targets)
  presses = Array.new(n_b, 0)
  free_vals = Array.new(free_vars.size, 0)
  best = INF
  best = enumerate(0, 0, best, free_vars, max_presses,
                   matrix, pivot_col, presses, free_vals)
  best == INF ? -1 : best
end

total = 0
File.each_line("input.txt") do |line|
  next if line.strip.empty?
  buttons, targets = parse_line(line)
  res = solve(buttons, targets)
  total += res if res > 0
end
puts total
