
input = File.readlines('input.txt').map(&:chomp)

def gcd(a, b)
  b == 0 ? a : gcd(b, a % b)
end

def count_visible_asteroids(map, x, y)
  count = 0
  map.each_with_index do |row, i|
    next if i == y
    row.chars.each_with_index do |char, j|
      next if char == '.' || j == x
      dx, dy = j - x, i - y
      count += 1 if gcd(dx.abs, dy.abs) == 1 && map[y][x] == '#'
    end
  end
  count
end

def vaporize_asteroids(map, x, y)
  vaporized = []
  while true
    visible_asteroids = []
    map.each_with_index do |row, i|
      row.chars.each_with_index do |char, j|
        next if char == '.' || (i == y && j == x)
        dx, dy = j - x, i - y
        if gcd(dx.abs, dy.abs) == 1
          angle = Math.atan2(dy, dx)
          angle += 2 * Math::PI if angle < 0
          distance = dx.abs + dy.abs
          visible_asteroids << [angle, distance, j, i]
        end
      end
    end
    visible_asteroids.sort!.each do |angle, distance, j, i|
      map[i][j] = '.'
      vaporized << [j, i]
    end
    break if vaporized.size >= 200
  end
  vaporized[199][0] * 100 + vaporized[199][1]
end

best_location = [0, 0]
max_visible = 0

input.each_with_index do |row, i|
  row.chars.each_with_index do |char, j|
    next if char == '.'
    visible = count_visible_asteroids(input, j, i)
    if visible > max_visible
      max_visible = visible
      best_location = [j, i]
    end
  end
end

puts max_visible
puts vaporize_asteroids(input, best_location[0], best_location[1])
