
def grid = new File("input.txt").readLines().collect { it as char[] }
def h = grid.size()
def w = grid[0].size()

def dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]]
def startX = -1, startY = -1, startDir = -1

for (int i = 0; i < h; i++) {
    for (int j = 0; j < w; j++) {
        switch (grid[i][j]) {
            case '^': startX = j; startY = i; startDir = 0; break
            case '>': startX = j; startY = i; startDir = 1; break
            case 'v': startX = j; startY = i; startDir = 2; break
            case '<': startX = j; startY = i; startDir = 3; break
        }
        if (startX != -1) break
    }
    if (startX != -1) break
}

def visited = new HashSet()
visited.add([startX, startY])
def x = startX
def y = startY
def dirIdx = startDir
def dirX = dirs[dirIdx][0]
def dirY = dirs[dirIdx][1]


while (true) {
    def nx = x + dirX
    def ny = y + dirY
    if (nx < 0 || nx >= w || ny < 0 || ny >= h) break
    if (grid[ny][nx] == '#') {
        dirIdx = (dirIdx + 1) % 4
        dirX = dirs[dirIdx][0]
        dirY = dirs[dirIdx][1]
        continue
    }
    x = nx
    y = ny
    visited.add([x, y])
}

println visited.size()
