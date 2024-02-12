
asteroids = File.read("input.txt").lines.map { |line| line.chars.map { |char| char == '#' } }

def count_visible_asteroids(asteroids, x, y)
  angles = Hash(Float64, Bool).new
  asteroids.each_with_index do |row, other_y|
    row.each_with_index do |is_asteroid, other_x|
      next if !is_asteroid || (other_x == x && other_y == y)
      angle = Math.atan2(other_y - y, other_x - x)
      angles[angle] = true
    end
  end
  angles.size
end

max_count = 0
asteroids.each_with_index do |row, y|
  row.each_with_index do |is_asteroid, x|
    next unless is_asteroid
    count = count_visible_asteroids(asteroids, x, y)
    max_count = count if count > max_count
  end
end

puts max_count
