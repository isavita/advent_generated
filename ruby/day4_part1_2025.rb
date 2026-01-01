#!/usr/bin/env ruby

if __FILE__ == $0
  lines = File.readlines('input.txt').map(&:chomp).reject(&:empty?)
  exit if lines.empty?
  rows = lines.size
  cols = lines[0].size
  grid = lines.map { |l| l.bytes }
  offsets = [-1, 0, 1].product([-1, 0, 1]) - [[0, 0]]
  accessible = 0
  rows.times do |y|
    cols.times do |x|
      next unless grid[y][x] == 64 # '@'.ord
      neighbors = offsets.count do |dy, dx|
        ny = y + dy
        nx = x + dx
        ny.between?(0, rows - 1) && nx.between?(0, cols - 1) && grid[ny][nx] == 64
      end
      accessible += 1 if neighbors < 4
    end
  end
  puts accessible
end