
# frozen_string_literal: true

def main
  lines = File.read_lines("input.txt")
  grid = lines.reject(&.empty?).map(&.chomp('\r'))

  height = grid.size
  if height == 0
    puts 0
    return
  end

  width = grid[0].size
  start_x = -1
  start_y = -1

  grid.each_with_index do |row, y|
    idx = row.index('S')
    if idx
      start_x = idx
      start_y = y
      break
    end
  end

  unless start_x >= 0
    abort "Start point 'S' not found"
  end

  counts = Hash(Int32, UInt64).new(0_u64)
  counts[start_x] = 1_u64

  (start_y...height).each do |y|
    next_counts = Hash(Int32, UInt64).new(0_u64)
    counts.each do |x, cnt|
      splitter = x >= 0 && x < width && grid[y][x] == '^'
      if splitter
        next_counts[x - 1] += cnt
        next_counts[x + 1] += cnt
      else
        next_counts[x] += cnt
      end
    end
    counts = next_counts
  end

  total = counts.values.reduce(0_u64) { |s, v| s + v }
  puts total
end

main
