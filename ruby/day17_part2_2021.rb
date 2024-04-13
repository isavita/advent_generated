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
x_range = parts[0][15..-1].split("..")
y_range = parts[1][2..-1].split("..")
x_min = x_range[0].to_i
x_max = x_range[1].to_i
y_min = y_range[0].to_i
y_max = y_range[1].to_i

velocities = {}
(-1000..1000).each do |x_vel|
  (-1000..1000).each do |y_vel|
    x_pos, y_pos = 0, 0
    cur_x_vel, cur_y_vel = x_vel, y_vel
    in_target_area = false
    loop do
      x_pos += cur_x_vel
      y_pos += cur_y_vel

      if x_pos.between?(x_min, x_max) && y_pos.between?(y_min, y_max)
        in_target_area = true
        break
      end

      break if is_moving_away(x_pos, y_pos, cur_x_vel, cur_y_vel, x_min, x_max, y_min, y_max)

      cur_x_vel -= 1 if cur_x_vel > 0
      cur_x_vel += 1 if cur_x_vel < 0
      cur_y_vel -= 1
    end

    if in_target_area
      velocity_key = "#{x_vel},#{y_vel}"
      velocities[velocity_key] = true
    end
  end
end

puts velocities.size