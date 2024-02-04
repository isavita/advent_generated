with open("input.txt", "r") as file:
    directions = file.read().strip().split(",")

dx = 0
dy = 0
max_dist = 0

for direction in directions:
    if direction == "n":
        dy += 1
    elif direction == "s":
        dy -= 1
    elif direction == "ne":
        dx += 1
    elif direction == "sw":
        dx -= 1
    elif direction == "nw":
        dx -= 1
        dy += 1
    elif direction == "se":
        dx += 1
        dy -= 1

    max_dist = max(max_dist, max(abs(dx), abs(dy)))

print(max(abs(dx), abs(dy)))
print(max_dist)