
with open("input.txt", "r") as file:
    line = file.readline().strip()
    parts = line.split(", ")
    x_range = parts[0][15:].split("..")
    y_range = parts[1][2:].split("..")
    x_min, x_max = int(x_range[0]), int(x_range[1])
    y_min, y_max = int(y_range[0]), int(y_range[1])

max_y = -1 << 30
for x_vel in range(-1000, 1001):
    for y_vel in range(-1000, 1001):
        x_pos, y_pos = 0, 0
        cur_x_vel, cur_y_vel = x_vel, y_vel
        highest_y = y_pos
        while True:
            x_pos += cur_x_vel
            y_pos += cur_y_vel

            if x_min <= x_pos <= x_max and y_min <= y_pos <= y_max:
                if highest_y > max_y:
                    max_y = highest_y
                break

            if x_pos < x_min and cur_x_vel < 0:
                break
            if x_pos > x_max and cur_x_vel > 0:
                break
            if y_pos < y_min and cur_y_vel < 0:
                break

            if cur_x_vel > 0:
                cur_x_vel -= 1
            elif cur_x_vel < 0:
                cur_x_vel += 1

            cur_y_vel -= 1
            if y_pos > highest_y:
                highest_y = y_pos

print(max_y)
