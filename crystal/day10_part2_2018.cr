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
  min_x, max_x, min_y, max_y = Int32::MAX, Int32::MIN, Int32::MAX, Int32::MIN
  stars.each do |star|
    x, y = star.x + star.vX * t, star.y + star.vY * t
    min_x, max_x = x, max_x if x < min_x
    max_x = x if x > max_x
    min_y, max_y = y, max_y if y < min_y
    max_y = y if y > max_y
  end
  area = (max_x - min_x + 1) + (max_y - min_y + 1)
  if area < smallest_area
    smallest_area = area
    smallest_t = t
  end
end

puts smallest_t

t = smallest_t
stars.each do |star|
  star.x += star.vX * t
  star.y += star.vY * t
end

min_x, max_x, min_y, max_y = Int32::MAX, Int32::MIN, Int32::MAX, Int32::MIN
stars.each do |star|
  min_x, max_x = star.x, max_x if star.x < min_x
  max_x = star.x if star.x > max_x
  min_y, max_y = star.y, max_y if star.y < min_y
  max_y = star.y if star.y > max_y
end

mapper = Array.new(max_y - min_y + 1) { Array.new(max_x - min_x + 1, false) }
stars.each do |star|
  mapper[star.y - min_y][star.x - min_x] = true
end

mapper.each do |row|
  row.each do |cell|
    # print cell ? '#' : ' '
  end
  # puts
end