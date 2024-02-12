
file = File.open("input.txt")
line = file.gets_to_end
parts = line.split(", ")
x_range = parts[0][15..].split("..")
y_range = parts[1][2..].split("..")
x_min = x_range[0].to_i
x_max = x_range[1].to_i
y_min = y_range[0].to_i
y_max = y_range[1].to_i

max_y = -1 << 30
(-1000..1000).each do |x_vel|
  (-1000..1000).each do |y_vel|
    x_pos = 0
    y_pos = 0
    cur_x_vel = x_vel
    cur_y_vel = y_vel
    highest_y = y_pos

    loop do
      x_pos += cur_x_vel
      y_pos += cur_y_vel

      if x_pos >= x_min && x_pos <= x_max && y_pos >= y_min && y_pos <= y_max
        max_y = highest_y if highest_y > max_y
        break
      end

      break if is_moving_away(x_pos, y_pos, cur_x_vel, cur_y_vel, x_min, x_max, y_min, y_max)

      cur_x_vel -= 1 if cur_x_vel > 0
      cur_x_vel += 1 if cur_x_vel < 0
      cur_y_vel -= 1
      highest_y = y_pos if y_pos > highest_y
    end
  end
end

puts max_y

def is_moving_away(x_pos, y_pos, x_vel, y_vel, x_min, x_max, y_min, y_max)
  return true if x_pos < x_min && x_vel < 0
  return true if x_pos > x_max && x_vel > 0
  return true if y_pos < y_min && y_vel < 0
  false
end
