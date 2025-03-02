
def grid = [:]
def startX = 0
def startY = 0
def lines = new File("input.txt").readLines()
for (int y = 0; y < lines.size(); y++) {
    def line = lines[y]
    for (int x = 0; x < line.size(); x++) {
        if (line[x] == '#') {
            grid[[x, y]] = 2
        }
    }
    startX = line.size().intdiv(2)
    startY = y.intdiv(2)
}
def dx = [0, 1, 0, -1]
def dy = [-1, 0, 1, 0]
def x = startX
def y = startY
def dir = 0
def infectedCount = 0
for (int i = 0; i < 10000000; i++) {
    def pos = [x, y]
    grid[pos] = grid.getOrDefault(pos, 0)
    if (grid[pos] == 0) {
        dir = (dir - 1 + 4) % 4
        grid[pos] = 1
    } else if (grid[pos] == 1) {
        grid[pos] = 2
        infectedCount++
    } else if (grid[pos] == 2) {
        dir = (dir + 1) % 4
        grid[pos] = 3
    } else if (grid[pos] == 3) {
        dir = (dir + 2) % 4
        grid[pos] = 0
    }
    x += dx[dir]
    y += dy[dir]
}
println(infectedCount)

