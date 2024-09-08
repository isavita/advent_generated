# Read input
input = File.read('input.txt').strip
x_min, x_max, y_min, y_max = input.scan(/-?\d+/).map(&:to_i)

def hits_target?(vx, vy, x_min, x_max, y_min, y_max)
  x, y = 0, 0
  while x <= x_max && y >= y_min
    return true if x >= x_min && x <= x_max && y >= y_min && y <= y_max
    x += vx
    y += vy
    vx = [vx - 1, 0].max
    vy -= 1
  end
  false
end

# Part 1
max_height = y_min.abs * (y_min.abs - 1) / 2
puts "Part 1: #{max_height}"

# Part 2
count = 0
(1..x_max).each do |vx|
  (y_min..y_min.abs).each do |vy|
    count += 1 if hits_target?(vx, vy, x_min, x_max, y_min, y_max)
  end
end
puts "Part 2: #{count}"
