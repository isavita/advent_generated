
with open("input.txt", "r") as file:
    lines = file.readlines()

grid = {}  # true: infected, false: clean
startX, startY = 0, 0
for y, line in enumerate(lines):
    for x, c in enumerate(line.strip()):
        if c == '#':
            grid[(x, y)] = True
    startX, startY = len(line.strip()) // 2, y // 2

# Directions: up, right, down, left
dx = [0, 1, 0, -1]
dy = [-1, 0, 1, 0]

x, y, dir = startX, startY, 0  # Start facing up
infectedCount = 0

for _ in range(10000):
    pos = (x, y)
    if grid.get(pos, False):  # infected
        dir = (dir + 1) % 4  # Turn right
        del grid[pos]  # Clean
    else:  # clean
        dir = (dir - 1 + 4) % 4  # Turn left
        grid[pos] = True  # Infect
        infectedCount += 1
    x, y = x + dx[dir], y + dy[dir]  # Move forward

print(infectedCount)
