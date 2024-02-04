with open("input.txt", "r") as file:
    directions = file.read().strip().split(",")

x = y = z = 0
max_distance = 0

for direction in directions:
    if direction == "n":
        y += 1
        z -= 1
    elif direction == "ne":
        x += 1
        z -= 1
    elif direction == "se":
        x += 1
        y -= 1
    elif direction == "s":
        y -= 1
        z += 1
    elif direction == "sw":
        x -= 1
        z += 1
    elif direction == "nw":
        x -= 1
        y += 1
    
    max_distance = max(max(abs(x), abs(y), abs(z)), max_distance)

result = max(abs(x), abs(y), abs(z))
print(result)