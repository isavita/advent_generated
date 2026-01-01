#!/usr/bin/env ruby
file = 'input.txt'
unless File.exist?(file)
  $stderr.puts 'File not found'
  exit 1
end
grid = File.readlines(file).map(&:chomp).reject(&:empty?)
if grid.empty?
  puts 0
  exit
end

h = grid.size
w = grid[0].length
sx = sy = nil
grid.each_with_index do |row, y|
  x = row.index('S')
  if x
    sx, sy = x, y
    break
  end
end
unless sx
  $stderr.puts "Start point 'S' not found"
  exit 1
end

counts = { sx => 1 }
(sy...h).each do |y|
  nxt = Hash.new(0)
  counts.each do |x, cnt|
    if x >= 0 && x < w && grid[y][x] == '^'
      nxt[x - 1] += cnt
      nxt[x + 1] += cnt
    else
      nxt[x] += cnt
    end
  end
  counts = nxt
end

total = counts.values.reduce(0, :+)
puts total