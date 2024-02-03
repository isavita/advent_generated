
require 'matrix'

points = File.readlines('input.txt').map do |line|
  line.scan(/-?\d+/).map(&:to_i)
end

def bounds(points)
  min_x = points.map { |p| p[0] }.min
  max_x = points.map { |p| p[0] }.max
  min_y = points.map { |p| p[1] }.min
  max_y = points.map { |p| p[1] }.max

  [min_x, max_x, min_y, max_y]
end

def display(points)
  min_x, max_x, min_y, max_y = bounds(points)
  grid = Array.new(max_y - min_y + 1) { Array.new(max_x - min_x + 1, '.') }

  points.each do |p|
    x = p[0] - min_x
    y = p[1] - min_y
    grid[y][x] = '#'
  end

  grid.map { |row| row.join('') }.join("\n")
end

def move(points)
  points.map { |p| [p[0] + p[2], p[1] + p[3], p[2], p[3]] }
end

def area(points)
  min_x, max_x, min_y, max_y = bounds(points)
  (max_x - min_x + 1) * (max_y - min_y + 1)
end

area_threshold = Float::INFINITY
seconds = 0

loop do
  new_points = move(points)
  new_area = area(new_points)

  if new_area < area_threshold
    area_threshold = new_area
    points = new_points
    seconds += 1
  else
    puts display(points)
    puts "Seconds: #{seconds}"
    break
  end
end
