require "math"

struct Asteroid
  property x, y, angle, dist

  def initialize(@x : Int32, @y : Int32, @angle : Float64, @dist : Float64)
  end
end

def read_asteroids(filename)
  asteroids = [] of Array(Bool)
  File.open(filename) do |file|
    file.each_line do |line|
      row = Array(Bool).new(line.size, false)
      line.chars.each_with_index do |char, i|
        row[i] = char == '#'
      end
      asteroids << row
    end
  end
  asteroids
end

def vaporize_asteroids(asteroids, station)
  targets = [] of Asteroid
  asteroids.each_with_index do |row, y|
    row.each_with_index do |is_asteroid, x|
      next if !is_asteroid || (x == station[0] && y == station[1])
      angle = Math.atan2(y - station[1], x - station[0])
      dist = Math.hypot(x - station[0], y - station[1])
      angle += 2 * Math::PI if angle < -Math::PI / 2
      targets << Asteroid.new(x, y, angle, dist)
    end
  end

  targets.sort_by! { |t| [t.angle, t.dist] }

  vaporized = [] of Asteroid
  while targets.size > 0
    last_angle = -Float64::MAX
    i = 0
    while i < targets.size
      if targets[i].angle != last_angle
        vaporized << targets[i]
        last_angle = targets[i].angle
        targets.delete_at(i)
      else
        i += 1
      end
    end
  end
  vaporized
end

def find_best_asteroid_location(asteroids)
  best_location = [0, 0]
  max_count = 0
  asteroids.each_with_index do |row, y|
    row.each_with_index do |is_asteroid, x|
      next if !is_asteroid
      count = count_visible_asteroids(asteroids, x, y)
      if count > max_count
        max_count = count
        best_location = [x, y]
      end
    end
  end
  {best_location, max_count}
end

def count_visible_asteroids(asteroids, x, y)
  angles = Set(Float64).new
  asteroids.each_with_index do |row, other_y|
    row.each_with_index do |is_asteroid, other_x|
      next if !is_asteroid || (other_x == x && other_y == y)
      angle = Math.atan2(other_y - y, other_x - x)
      angles << angle
    end
  end
  angles.size
end

asteroids = read_asteroids("input.txt")
station, _ = find_best_asteroid_location(asteroids)
vaporized = vaporize_asteroids(asteroids, station)
if vaporized.size >= 200
  result = vaporized[199].x * 100 + vaporized[199].y
  puts result
else
  puts "Less than 200 asteroids were vaporized."
end