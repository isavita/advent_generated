import strutils

var file = open("input.txt")
let lines = file.readAll().splitLines()

var grid: seq[seq[char]] = @[]
for line in lines:
    grid.add(@[])
    for ch in line:
        grid[^1].add(ch)

for j in 0..<grid[0].len:
    var i = 0
    while i < grid.len:
        if grid[i][j] == 'O':
            var k = i
            while k > 0 and grid[k-1][j] == '.':
                grid[k-1][j] = 'O'
                grid[k][j] = '.'
                k -= 1
        i += 1

var totalLoad = 0
for i in 0..<grid.len:
    for j in 0..<grid[i].len:
        if grid[i][j] == 'O':
            totalLoad += grid.len - i

echo totalLoad