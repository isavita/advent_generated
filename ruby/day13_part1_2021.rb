file = File.open("input.txt")
points = {}
folds = []
reading_points = true

file.each_line do |line|
  if line == "\n"
    reading_points = false
    next
  end

  if reading_points
    coords = line.split(",")
    x = coords[0].to_i
    y = coords[1].to_i
    points[[x, y]] = true
  else
    folds << line
  end
end

fold = folds[0].split(" ")[2]
axis_value = fold.split("=")
axis = axis_value[0]
value = axis_value[1].to_i

new_points = {}

if axis == "x"
  points.each do |point, _|
    if point[0] > value
      point[0] = 2 * value - point[0]
    end
    new_points[point] = true
  end
else
  points.each do |point, _|
    if point[1] > value
      point[1] = 2 * value - point[1]
    end
    new_points[point] = true
  end
end

puts new_points.length