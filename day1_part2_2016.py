
f = open("input.txt", "r")
instructions = f.read().split(", ")

direction = 0
x, y = 0, 0
visited = set()
visited.add((x, y))
found = False

for instruction in instructions:
    if instruction[0] == "R":
        direction = (direction + 1) % 4
    else:
        direction = (direction - 1) % 4

    distance = int(instruction[1:])

    for _ in range(distance):
        if direction == 0:
            y += 1
        elif direction == 1:
            x += 1
        elif direction == 2:
            y -= 1
        else:
            x -= 1

        if not found and (x, y) in visited:
            print(abs(x) + abs(y))
            found = True

        visited.add((x, y))

print(abs(x) + abs(y))
