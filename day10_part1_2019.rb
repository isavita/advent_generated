
input = File.readlines("input.txt").map(&:strip)

asteroids = []
input.each_with_index do |row, y|
  row.chars.each_with_index do |char, x|
    asteroids << [x, y] if char == "#"
  end
end

max_detected = 0
best_location = nil

asteroids.each do |station|
  detected = Hash.new(0)
  asteroids.each do |asteroid|
    next if station == asteroid

    dx = asteroid[0] - station[0]
    dy = asteroid[1] - station[1]
    gcd = dx.gcd(dy)
    key = [dx / gcd, dy / gcd]

    detected[key] += 1
  end

  if detected.keys.length > max_detected
    max_detected = detected.keys.length
    best_location = station
  end
end

puts max_detected
