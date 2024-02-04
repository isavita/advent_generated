
def parse():
    res = [False] * 25

    with open("input.txt", "r") as file:
        for row, line in enumerate(file):
            for col, char in enumerate(line.strip()):
                if char == '#':
                    res[row * 5 + col] = True
    return res

def next2(space):
    new_space = {}

    min_level, max_level = min_max_level(space)

    for level in range(min_level - 1, max_level + 2):
        new_space[level] = [False] * 25

        for cell in range(25):

            if cell == 12:
                continue

            row, col = cell // 5, cell % 5
            neighbours = 0

            if row == 0:
                if infested(space, level - 1, 7):
                    neighbours += 1

            if col == 0:
                if infested(space, level - 1, 11):
                    neighbours += 1

            if col == 4:
                if infested(space, level - 1, 13):
                    neighbours += 1

            if row == 4:
                if infested(space, level - 1, 17):
                    neighbours += 1

            if cell == 7:
                for i in range(5):
                    if infested(space, level + 1, i):
                        neighbours += 1

            if cell == 11:
                for i in range(5):
                    if infested(space, level + 1, 5 * i):
                        neighbours += 1

            if cell == 13:
                for i in range(5):
                    if infested(space, level + 1, 5 * i + 4):
                        neighbours += 1

            if cell == 17:
                for i in range(5):
                    if infested(space, level + 1, 20 + i):
                        neighbours += 1

            if row > 0 and cell != 17:
                if infested(space, level, cell - 5):
                    neighbours += 1

            if col > 0 and cell != 13:
                if infested(space, level, cell - 1):
                    neighbours += 1

            if col < 4 and cell != 11:
                if infested(space, level, cell + 1):
                    neighbours += 1

            if row < 4 and cell != 7:
                if infested(space, level, cell + 5):
                    neighbours += 1

            if infested(space, level, cell) and neighbours != 1:
                new_space[level][cell] = False
                continue

            if not infested(space, level, cell) and neighbours in [1, 2]:
                new_space[level][cell] = True
                continue

            new_space[level][cell] = infested(space, level, cell)

    clean(new_space)

    return new_space

def clean(space):
    min_level, max_level = min_max_level(space)

    count_min, count_max = 0, 0
    for cell in range(25):
        if space[min_level][cell]:
            count_min += 1
        if space[max_level][cell]:
            count_max += 1

    if count_min == 0:
        del space[min_level]
    if count_max == 0:
        del space[max_level]

def infested(space, level, cell):
    if level not in space:
        return False
    return space[level][cell]

def min_max_level(space):
    min_level, max_level = float('inf'), float('-inf')
    for level in space:
        min_level = min(min_level, level)
        max_level = max(max_level, level)
    return min_level, max_level

input_data = parse()

space = {0: input_data}

for _ in range(200):
    space = next2(space)

count = 0
for grid in space.values():
    count += sum(grid)

print(count)
