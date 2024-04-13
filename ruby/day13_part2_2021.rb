file = File.open("input.txt")
points = {}
folds = []
reading_points = true

file.each_line do |line|
  line.strip!
  if line.empty?
    reading_points = false
    next
  end

  if reading_points
    x, y = line.split(",").map(&:to_i)
    points[[x, y]] = true
  else
    axis, value = line.split("=")
    value = value.to_i
    if axis.include?("x")
      folds << [value, 0]
    else
      folds << [0, value]
    end
  end
end
file.close

folds.each_with_index do |fold, index|
  new_points = {}
  points.each_key do |x, y|
    new_x, new_y = x, y
    new_x = fold[0] - (x - fold[0]) if fold[0] != 0 && x > fold[0]
    new_y = fold[1] - (y - fold[1]) if fold[1] != 0 && y > fold[1]
    new_points[[new_x, new_y]] = true
  end
  points = new_points
  if index == 0
    puts "Number of dots visible after first fold: #{points.size}"
  end
end

max_x = points.keys.map(&:first).max
max_y = points.keys.map(&:last).max

grid = Array.new(max_y + 1) { Array.new(max_x + 1, ' ') }
points.each_key { |x, y| grid[y][x] = '#' }

grid.each { |row| puts row.join }