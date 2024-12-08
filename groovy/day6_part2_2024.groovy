
import java.util.regex.Matcher
import java.util.regex.Pattern

def grid = new File("input.txt").readLines().collect { it.toCharArray() }
def h = grid.size()
def w = grid[0].size()

def startX = -1, startY = -1, startDir = -1
for (int i = 0; i < h; i++) {
    for (int j = 0; j < w; j++) {
        switch (grid[i][j]) {
            case '^': startX = j; startY = i; startDir = 0; break
            case '>': startX = j; startY = i; startDir = 1; break
            case 'v': startX = j; startY = i; startDir = 2; break
            case '<': startX = j; startY = i; startDir = 3; break
        }
    }
}
grid[startY][startX] = '.'

def canLoop = 0
for (int y = 0; y < h; y++) {
    for (int x = 0; x < w; x++) {
        if (x == startX && y == startY) continue
        if (grid[y][x] != '.') continue
        grid[y][x] = '#'
        if (loops(grid, startX, startY, startDir)) canLoop++
        grid[y][x] = '.'
    }
}

println canLoop


boolean loops(grid, sx, sy, sdir) {
    def h = grid.size()
    def w = grid[0].size()
    def dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]]
    def x = sx
    def y = sy
    def dir = sdir
    def seen = [:]
    for (int step = 0; step < 2000000; step++) {
        def st = "${x},${y},${dir}"
        if (seen[st]) return true
        seen[st] = true
        def dx = dirs[dir][0]
        def dy = dirs[dir][1]
        def nx = x + dx
        def ny = y + dy
        if (nx < 0 || nx >= w || ny < 0 || ny >= h) return false
        if (grid[ny][nx] == '#') {
            dir = (dir + 1) % 4
            continue
        }
        x = nx
        y = ny
    }
    return false
}
