import strutils, sequtils

let file = readFile("input.txt")
var grid: seq[seq[char]]
for line in file.splitLines():
    grid.add(line.toSeq())

var x, y: int
for i in 0..<grid[0].len:
    if grid[0][i] == '|':
        x = i
        break

var dx, dy: int
var steps: int
dx = 0
dy = 1

while true:
    if x < 0 or x >= grid[0].len or y < 0 or y >= grid.len:
        break

    let cell = grid[y][x]

    if cell == ' ':
        break

    inc(steps)

    if cell == '+':
        if dx == 0:
            if x > 0 and (grid[y][x-1] == '-' or (grid[y][x-1] >= 'A' and grid[y][x-1] <= 'Z')):
                (dx, dy) = (-1, 0)
            else:
                (dx, dy) = (1, 0)
        else:
            if y > 0 and (grid[y-1][x] == '|' or (grid[y-1][x] >= 'A' and grid[y-1][x] <= 'Z')):
                (dx, dy) = (0, -1)
            else:
                (dx, dy) = (0, 1)

    x += dx
    y += dy

echo steps