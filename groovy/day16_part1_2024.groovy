
import java.util.PriorityQueue

def grid = []
new File("input.txt").eachLine { grid << it }

def n = grid.size()
def m = grid[0].size()
def sx, sy, ex, ey
for (i in 0..<n) {
    for (j in 0..<m) {
        if (grid[i][j] == 'S') {
            sx = i
            sy = j
        } else if (grid[i][j] == 'E') {
            ex = i
            ey = j
        }
    }
}

def dx = [-1, 0, 1, 0]
def dy = [0, 1, 0, -1]

def dist = (0..<n).collect { (0..<m).collect { [Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE] } }
dist[sx][sy][1] = 0

def pq = new PriorityQueue<List<Integer>>({ a, b -> a[3] <=> b[3] })
pq.add([sx, sy, 1, 0])

while (!pq.isEmpty()) {
    def u = pq.poll()
    if (dist[u[0]][u[1]][u[2]] < u[3]) {
        continue
    }
    if (u[0] == ex && u[1] == ey) {
        println u[3]
        return
    }
    for (ndir in [(u[2] + 1) % 4, (u[2] + 3) % 4]) {
        def nc = u[3] + 1000
        if (nc < dist[u[0]][u[1]][ndir]) {
            dist[u[0]][u[1]][ndir] = nc
            pq.add([u[0], u[1], ndir, nc])
        }
    }
    def nx = u[0] + dx[u[2]]
    def ny = u[1] + dy[u[2]]
    if (nx >= 0 && nx < n && ny >= 0 && ny < m && grid[nx][ny] != '#') {
        def nc = u[3] + 1
        if (nc < dist[nx][ny][u[2]]) {
            dist[nx][ny][u[2]] = nc
            pq.add([nx, ny, u[2], nc])
        }
    }
}
