def parse_input(file)
  File.read(file).match(/x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)/).to_a[1..-1].map(&:to_i)
end

def simulate(vx, vy, x1, x2, y1, y2)
  x, y = 0, 0
  max_y = 0
  loop do
    x += vx
    y += vy
    max_y = [max_y, y].max
    vx = [vx - 1, 0].max
    vy -= 1
    return max_y if x >= x1 && x <= x2 && y >= y1 && y <= y2
    return nil if x > x2 || y < y1
  end
end

x1, x2, y1, y2 = parse_input('input.txt')

max_vx = x2
min_vx = (Math.sqrt(8 * x1 + 1) - 1) / 2
max_vy = y1.abs - 1
min_vy = y1

highest_y = 0

(min_vx.ceil..max_vx).each do |vx|
  (min_vy..max_vy).each do |vy|
    if max_y = simulate(vx, vy, x1, x2, y1, y2)
      highest_y = [highest_y, max_y].max
    end
  end
end

puts highest_y
