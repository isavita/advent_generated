import re
import sys

def main():
    with open("input.txt", "r") as file:
        input_str = file.read().strip()
    lines = re.split("\n", input_str)

    ground = [['+']]
    max_x, min_x, max_y, min_y = 0, 0, 0, 20
    x_offset, y_offset = 500, 0

    for line in lines:
        split = re.split("[=, .]+", line)
        if split[0] == "x":
            x = int(split[1]) - x_offset
            y1 = int(split[3]) - y_offset
            y2 = int(split[4]) - y_offset

            while x >= max_x:
                max_x += 1
                for j in range(len(ground)):
                    ground[j].append('.')
            while x <= min_x:
                min_x -= 1
                for j in range(len(ground)):
                    ground[j].insert(0, '.')
            while y2 > max_y:
                max_y += 1
                ground.append(['.' for _ in range(len(ground[0]))])
            if y1 < min_y:
                min_y = y1
            for i in range(y1, y2 + 1):
                ground[i][x - min_x] = '#'

        else:
            y = int(split[1]) - y_offset
            x1 = int(split[3]) - x_offset
            x2 = int(split[4]) - x_offset

            while y > max_y:
                max_y += 1
                ground.append(['.' for _ in range(len(ground[0]))])
            while x2 >= max_x:
                max_x += 1
                for j in range(len(ground)):
                    ground[j].append('.')
            while x1 <= min_x:
                min_x -= 1
                for j in range(len(ground)):
                    ground[j].insert(0, '.')
            for i in range(x1, x2 + 1):
                ground[y][i - min_x] = '#'
            if y < min_y:
                min_y = y

    water_count = 0
    flow_count = 0
    round_limit = 200000

    while ground[1][-min_x] != '|' and water_count < round_limit:
        can_move = True
        x = -min_x
        y = 1
        try_left = 0
        while can_move:
            if y + 1 > max_y or ground[y + 1][x] == '|':
                ground[y][x] = '|'
                can_move = False
                if y >= min_y:
                    flow_count += 1
            elif ground[y + 1][x] == '.':
                y += 1
                try_left = 0
            elif ground[y + 1][x] == '#' or ground[y + 1][x] == '~':
                if (try_left == 1 and ground[y][x - 1] == '|') or \
                   (try_left == 2 and ground[y][x + 1] == '|') or \
                   (ground[y][x + 1] == '|' and ground[y][x - 1] != '.') or \
                   (ground[y][x + 1] != '.' and ground[y][x - 1] == '|'):
                    ground[y][x] = '|'
                    flow_count += 1
                    can_move = False
                    i = x + 1
                    while ground[y][i] == '~':
                        ground[y][i] = '|'
                        water_count -= 1
                        flow_count += 1
                        i += 1
                    i = x - 1
                    while ground[y][i] == '~':
                        ground[y][i] = '|'
                        water_count -= 1
                        flow_count += 1
                        i -= 1
                elif (try_left == 0 and ground[y][x - 1] == '.') or \
                     (try_left == 1 and ground[y][x - 1] == '.'):
                    x -= 1
                    try_left = 1
                elif (try_left == 0 and ground[y][x + 1] == '.') or \
                     (try_left == 2 and ground[y][x + 1] == '.'):
                    x += 1
                    try_left = 2
                else:
                    can_move = False
                    ground[y][x] = '~'
                    water_count += 1

    print(water_count)

if __name__ == "__main__":
    main()
