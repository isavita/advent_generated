
require 'set'

def main
  lines = File.readlines('input.txt', chomp: true).reject(&:empty?)
  return puts 'Empty grid' if lines.empty?

  height = lines.size
  width  = lines[0].size

  start_x = start_y = nil
  lines.each_with_index do |row, y|
    if (i = row.index('S'))
      start_x = i
      start_y = y
      break
    end
  end
  unless start_x
    $stderr.puts "Start point 'S' not found"
    return
  end

  active = Set[start_x]
  total_splits = 0

  (start_y...height).each do |y|
    break if active.empty?
    next_active = Set.new
    active.each do |x|
      next if x < 0 || x >= width
      cell = lines[y][x]
      if cell == '^'
        total_splits += 1
        next_active << x - 1 if x - 1 >= 0
        next_active << x + 1 if x + 1 < width
      else
        next_active << x
      end
    end
    active = next_active
  end

  puts total_splits
end

main
