
with open('input.txt', 'r') as file:
    directions = file.read()

houses = set()
x, y = 0, 0
houses.add((x, y))

for direction in directions:
    if direction == '^':
        y += 1
    elif direction == 'v':
        y -= 1
    elif direction == '>':
        x += 1
    elif direction == '<':
        x -= 1
    houses.add((x, y))

print(len(houses))
