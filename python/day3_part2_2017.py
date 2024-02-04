
with open("input.txt", "r") as file:
    target = int(file.read().strip())

grid = {(0, 0): 1}
x, y = 0, 0
dx, dy = 0, -1

while True:
    if x == y or (x < 0 and x == -y) or (x > 0 and x == 1 - y):
        dx, dy = -dy, dx

    x += dx
    y += dy

    value = sum([grid.get((x + dx, y + dy), 0) for dx in range(-1, 2) for dy in range(-1, 2)])
    grid[(x, y)] = value

    if value > target:
        print(value)
        break
