
import re

def reg_split(text, delimiter):
    return re.split(delimiter, text)

def str_to_int(s):
    return int(s)

def main():
    with open("input.txt", "r") as file:
        input_str = file.read().strip()
    lines = reg_split(input_str, "\n")

    ground = [['+']]
    maxX, minX, maxY, minY = 0, 0, 0, 20
    xOffset, yOffset = 500, 0

    for line in lines:
        split = reg_split(line, "[=, .]+")
        if split[0] == "x":
            x = str_to_int(split[1]) - xOffset
            y1 = str_to_int(split[3]) - yOffset
            y2 = str_to_int(split[4]) - yOffset

            while x >= maxX:
                maxX += 1
                for j in ground:
                    j.append('.')
            while x <= minX:
                minX -= 1
                for j in ground:
                    j.insert(0, '.')
            while y2 > maxY:
                maxY += 1
                ground.append(['.'] * len(ground[0]))
            if y1 < minY:
                minY = y1
            for i in range(y1, y2 + 1):
                ground[i][x - minX] = '#'

        else:
            y = str_to_int(split[1]) - yOffset
            x1 = str_to_int(split[3]) - xOffset
            x2 = str_to_int(split[4]) - xOffset

            while y > maxY:
                maxY += 1
                ground.append(['.'] * len(ground[0]))
            while x2 >= maxX:
                maxX += 1
                for j in ground:
                    j.append('.')
            while x1 <= minX:
                minX -= 1
                for j in ground:
                    j.insert(0, '.')
            for i in range(x1, x2 + 1):
                ground[y][i - minX] = '#'
            if y < minY:
                minY = y

    water_count = 0
    flow_count = 0
    round_limit = 200000

    while ground[1][-minX] != '|' and water_count < round_limit:
        can_move = True
        x = -minX
        y = 1
        try_left = 0
        while can_move:
            if y + 1 > maxY or ground[y + 1][x] == '|':
                ground[y][x] = '|'
                can_move = False
                if y >= minY:
                    flow_count += 1
            elif ground[y + 1][x] == '.':
                y += 1
                try_left = 0
            elif ground[y + 1][x] in ['#', '~']:
                if ((try_left == 1 and ground[y][x - 1] == '|') or
                    (try_left == 2 and ground[y][x + 1] == '|') or
                    (ground[y][x + 1] == '|' and ground[y][x - 1] != '.') or
                    (ground[y][x + 1] != '.' and ground[y][x - 1] == '|')):
                    ground[y][x] = '|'
                    flow_count += 1
                    can_move = False
                    for i in range(x + 1, len(ground[0])):
                        if ground[y][i] == '~':
                            ground[y][i] = '|'
                            water_count -= 1
                            flow_count += 1
                        else:
                            break
                    for i in range(x - 1, -1, -1):
                        if ground[y][i] == '~':
                            ground[y][i] = '|'
                            water_count -= 1
                            flow_count += 1
                        else:
                            break
                elif ((try_left == 0 and ground[y][x - 1] == '.') or
                      (try_left == 1 and ground[y][x - 1] == '.')):
                    x -= 1
                    try_left = 1
                elif ((try_left == 0 and ground[y][x + 1] == '.') or
                      (try_left == 2 and ground[y][x + 1] == '.')):
                    x += 1
                    try_left = 2
                else:
                    can_move = False
                    ground[y][x] = '~'
                    water_count += 1

    print(flow_count + water_count)

if __name__ == "__main__":
    main()
