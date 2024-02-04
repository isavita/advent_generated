from collections import defaultdict

with open('input.txt', 'r') as file:
    directions = file.read()

houses = defaultdict(int)
x, y = 0, 0
rx, ry = 0, 0
houses[(x, y)] += 2

for i in range(len(directions)):
    if i % 2 == 0:
        if directions[i] == '^':
            y += 1
        elif directions[i] == 'v':
            y -= 1
        elif directions[i] == '>':
            x += 1
        elif directions[i] == '<':
            x -= 1
        houses[(x, y)] += 1
    else:
        if directions[i] == '^':
            ry += 1
        elif directions[i] == 'v':
            ry -= 1
        elif directions[i] == '>':
            rx += 1
        elif directions[i] == '<':
            rx -= 1
        houses[(rx, ry)] += 1

answer = len(houses)
print(answer)