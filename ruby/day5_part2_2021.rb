
def mark_line(grid, x1, y1, x2, y2)
  x_step = x1 == x2 ? 0 : x1 < x2 ? 1 : -1
  y_step = y1 == y2 ? 0 : y1 < y2 ? 1 : -1
  x, y = x1, y1
  loop do
    grid[y][x] += 1
    break if x == x2 && y == y2
    x += x_step
    y += y_step
  end
end

def count_overlaps(grid)
  grid.sum { |row| row.count { |point| point >= 2 } }
end

lines = File.readlines("input.txt").map { |line| line.split(" -> ").map { |point| point.split(",").map(&:to_i) } }

max_x = lines.flatten(1).max_by { |x, _| x }[0]
max_y = lines.flatten(1).max_by { |_, y| y }[1]

grid = Array.new(max_y + 1) { Array.new(max_x + 1, 0) }

lines.each do |(x1, y1), (x2, y2)|
  mark_line(grid, x1, y1, x2, y2) if x1 == x2 || y1 == y2 || (x1 - x2).abs == (y1 - y2).abs
end

puts count_overlaps(grid)
