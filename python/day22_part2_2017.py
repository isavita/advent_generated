
grid = {}
startX = 0
startY = 0

with open("input.txt", "r") as file:
    for y, line in enumerate(file):
        for x, c in enumerate(line.strip()):
            if c == '#':
                grid[(x, y)] = 2
        startX, startY = len(line.strip()) // 2, y // 2

dx = [0, 1, 0, -1]
dy = [-1, 0, 1, 0]

x = startX
y = startY
dir = 0
infectedCount = 0

for i in range(10000000):
    pos = (x, y)
    if pos not in grid:
        grid[pos] = 0

    if grid[pos] == 0:
        dir = (dir - 1 + 4) % 4
        grid[pos] = 1
    elif grid[pos] == 1:
        grid[pos] = 2
        infectedCount += 1
    elif grid[pos] == 2:
        dir = (dir + 1) % 4
        grid[pos] = 3
    elif grid[pos] == 3:
        dir = (dir + 2) % 4
        grid[pos] = 0

    x += dx[dir]
    y += dy[dir]

print(infectedCount)
