
require "json"

points = {} of Array(Int32) => Nil
folds = [] of Array(Int32)

File.open("input.txt", "r") do |file|
  reading_points = true
  file.each_line do |line|
    line = line.chomp
    if line.empty?
      reading_points = false
      next
    end
    if reading_points
      x, y = line.split(",").map(&.to_i)
      points[[x, y]] = nil
    else
      axis, val = line.split("=")
      folds << (axis.includes?("x") ? [val.to_i, 0] : [0, val.to_i])
    end
  end
end

folds.each_with_index do |fold, i|
  new_points = {} of Array(Int32) => Nil
  points.each_key do |point|
    new_point = point.clone
    if fold[0] != 0 && point[0] > fold[0]
      new_point[0] = fold[0] - (point[0] - fold[0])
    elsif fold[1] != 0 && point[1] > fold[1]
      new_point[1] = fold[1] - (point[1] - fold[1])
    end
    new_points[new_point] = nil
  end
  points = new_points
  puts "Number of dots visible after first fold: #{points.size}" if i == 0
end

max_x = points.keys.map(&.first).max
max_y = points.keys.map(&.last).max

grid = Array.new(max_y + 1) { Array.new(max_x + 1, ' ') }
points.each_key do |point|
  grid[point[1]][point[0]] = '#'
end

grid.each do |row|
  puts row.join
end
