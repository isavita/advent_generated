
lights = Array.new(1000) { Array.new(1000, 0) }

File.open("input.txt").each do |line|
  action, start_x, start_y, end_x, end_y = line.match(/(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)/).captures
  start_x, start_y, end_x, end_y = start_x.to_i, start_y.to_i, end_x.to_i, end_y.to_i

  (start_x..end_x).each do |x|
    (start_y..end_y).each do |y|
      case action
      when "turn on"
        lights[x][y] += 1
      when "turn off"
        lights[x][y] = [lights[x][y] - 1, 0].max
      when "toggle"
        lights[x][y] += 2
      end
    end
  end
end

puts lights.flatten.sum
