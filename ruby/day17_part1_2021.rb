def is_moving_away(x_pos, y_pos, x_vel, y_vel, x_min, x_max, y_min, y_max)
  return true if x_pos < x_min && x_vel < 0
  return true if x_pos > x_max && x_vel > 0
  return true if y_pos < y_min && y_vel < 0
  false
end

file = File.open("input.txt")
line = file.readline.strip
file.close

parts = line.split(", ")
x_range = parts[0][15..].split("..").map(&:to_i)
y_range = parts[1][2..].split("..").map(&:to_i)

x_min, x_max = x_range
y_min, y_max = y_range

max_y = -Float::INFINITY
(-1000..1000).each do |x_vel|
  (-1000..1000).each do |y_vel|
    x_pos, y_pos = 0, 0
    cur_x_vel, cur_y_vel = x_vel, y_vel
    highest_y = y_pos

    loop do
      x_pos += cur_x_vel
      y_pos += cur_y_vel
      highest_y = [highest_y, y_pos].max

      if x_pos.between?(x_min, x_max) && y_pos.between?(y_min, y_max)
        max_y = [max_y, highest_y].max
        break
      end

      break if is_moving_away(x_pos, y_pos, cur_x_vel, cur_y_vel, x_min, x_max, y_min, y_max)

      cur_x_vel -= 1 if cur_x_vel.positive?
      cur_x_vel += 1 if cur_x_vel.negative?
      cur_y_vel -= 1
    end
  end
end

puts max_y