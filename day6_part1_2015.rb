
grid = Array.new(1000) { Array.new(1000, false) }

File.foreach("input.txt") do |line|
  action, from, to = line.scan(/(toggle|turn on|turn off) (\d+,\d+) through (\d+,\d+)/).flatten
  x1, y1 = from.split(",").map(&:to_i)
  x2, y2 = to.split(",").map(&:to_i)

  (x1..x2).each do |x|
    (y1..y2).each do |y|
      case action
      when "turn on"
        grid[x][y] = true
      when "turn off"
        grid[x][y] = false
      when "toggle"
        grid[x][y] = !grid[x][y]
      end
    end
  end
end

puts grid.flatten.count(true)
