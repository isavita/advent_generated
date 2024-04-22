require "file_utils"

class Star
  property x : Int32
  property y : Int32
  property vX : Int32
  property vY : Int32
  property next : Star?

  def initialize(@x, @y, @vX, @vY)
  end
end

stars = [] of Star

File.read_lines("input.txt").each do |line|
  if match = /position=<\s*(-?\d+),\s*(-?\d+)>\s*velocity=<\s*(-?\d+),\s*(-?\d+)>/.match(line)
    x, y, vX, vY = match[1].to_i, match[2].to_i, match[3].to_i, match[4].to_i
    stars << Star.new(x, y, vX, vY)
  end
end

smallest_t = 0
smallest_area = Int32::MAX

(1..100000).each do |t|
  max_x = Int32::MIN
  min_x = Int32::MAX
  max_y = Int32::MIN
  min_y = Int32::MAX

  stars.each do |star|
    x = star.x + star.vX * t
    y = star.y + star.vY * t
    max_x = x if x > max_x
    min_x = x if x < min_x
    max_y = y if y > max_y
    min_y = y if y < min_y
  end

  len_x = max_x - min_x + 1
  len_y = max_y - min_y + 1
  area = len_x + len_y

  if area < smallest_area
    smallest_area = area
    smallest_t = t
  end
end

t = smallest_t

stars.each do |star|
  star.x += star.vX * t
  star.y += star.vY * t
end

max_x = Int32::MIN
min_x = Int32::MAX
max_y = Int32::MIN
min_y = Int32::MAX

stars.each do |star|
  max_x = star.x if star.x > max_x
  min_x = star.x if star.x < min_x
  max_y = star.y if star.y > max_y
  min_y = star.y if star.y < min_y
end

mapper = Array.new(max_y - min_y + 1) { Array.new(max_x - min_x + 1, false) }

stars.each do |star|
  mapper[star.y - min_y][star.x - min_x] = true
end

mapper.each do |row|
  row.each do |cell|
    print cell ? "#" : " "
  end
  puts
end