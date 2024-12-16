
def grid = new File("input.txt").readLines()
def n = grid.size()
def m = grid[0].size()
def sx, sy, ex, ey
for (i in 0..<n) {
    for (j in 0..<m) {
        if (grid[i][j] == 'S') {
            sx = i; sy = j
        } else if (grid[i][j] == 'E') {
            ex = i; ey = j
        }
    }
}

def dx = [-1, 0, 1, 0]
def dy = [0, 1, 0, -1]

def dist = (0..<n).collect { (0..<m).collect { [Integer.MAX_VALUE] * 4 } }
dist[sx][sy][1] = 0

def h = new PriorityQueue({ a, b -> a.cost <=> b.cost })
h.add([x: sx, y: sy, d: 1, cost: 0])

while (!h.isEmpty()) {
    def u = h.poll()
    if (dist[u.x][u.y][u.d] < u.cost) continue
    if (u.x == ex && u.y == ey) continue

    for (ndir in [(u.d + 1) % 4, (u.d + 3) % 4]) {
        def nc = u.cost + 1000
        if (nc < dist[u.x][u.y][ndir]) {
            dist[u.x][u.y][ndir] = nc
            h.add([x: u.x, y: u.y, d: ndir, cost: nc])
        }
    }
    def nx = u.x + dx[u.d]
    def ny = u.y + dy[u.d]
    if (nx >= 0 && nx < n && ny >= 0 && ny < m && grid[nx][ny] != '#') {
        def nc = u.cost + 1
        if (nc < dist[nx][ny][u.d]) {
            dist[nx][ny][u.d] = nc
            h.add([x: nx, y: ny, d: u.d, cost: nc])
        }
    }
}

def best = dist[ex][ey].min()

def used = (0..<n).collect { [false] * m }

def rev = []
for (d in 0..<4) {
    if (dist[ex][ey][d] == best) {
        rev << [x: ex, y: ey, d: d]
    }
}

def vis = (0..<n).collect { (0..<m).collect { [false] * 4 } }
rev.each { vis[it.x][it.y][it.d] = true }

while (rev) {
    def u = rev.remove(rev.size() - 1)
    used[u.x][u.y] = true

    def costU = dist[u.x][u.y][u.d]

    for (pd in [(u.d + 1) % 4, (u.d + 3) % 4]) {
        if (dist[u.x][u.y][pd] == costU - 1000 && !vis[u.x][u.y][pd]) {
            vis[u.x][u.y][pd] = true
            rev << [x: u.x, y: u.y, d: pd]
        }
    }

    def px = u.x - dx[u.d]
    def py = u.y - dy[u.d]
    if (px >= 0 && px < n && py >= 0 && py < m && grid[px][py] != '#' && dist[px][py][u.d] == costU - 1 && !vis[px][py][u.d]) {
        vis[px][py][u.d] = true
        rev << [x: px, y: py, d: u.d]
    }
}

def cnt = 0
for (i in 0..<n) {
    for (j in 0..<m) {
        if (used[i][j] && grid[i][j] != '#') {
            cnt++
        }
    }
}

println cnt
