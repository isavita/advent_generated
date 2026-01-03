
def main
  lines = File.read_lines("input.txt")
  height = lines.size
  width = lines[0].size

  sx = sy = 0
  lines.each_with_index do |row, y|
    if (i = row.index('S'))
      sx = i
      sy = y
      break
    end
  end

  active = Array.new(width, false)
  next_active = Array.new(width, false)
  active[sx] = true
  splits = 0

  (sy...height).each do |y|
    next_active.fill(false)
    width.times do |x|
      next unless active[x]
      if lines[y][x] == '^'
        splits += 1
        next_active[x - 1] = true if x > 0
        next_active[x + 1] = true if x + 1 < width
      else
        next_active[x] = true
      end
    end
    active, next_active = next_active, active
    break unless active.any?
  end

  puts splits
end

main
