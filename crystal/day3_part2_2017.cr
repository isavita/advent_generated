File.open("input.txt", "r") do |file|
  target = file.gets.not_nil!.to_i
  grid = Hash(Tuple(Int32, Int32), Int32).new(0)
  grid[{0, 0}] = 1
  x, y = 0, 0
  dx, dy = 0, -1

  loop do
    if x == y || (x < 0 && x == -y) || (x > 0 && x == 1 - y)
      dx, dy = -dy, dx
    end
    x += dx
    y += dy
    value = 0
    (-1..1).each do |dx|
      (-1..1).each do |dy|
        value += grid[{x + dx, y + dy}]
      end
    end
    grid[{x, y}] = value
    if value > target
      puts value
      break
    end
  end
end