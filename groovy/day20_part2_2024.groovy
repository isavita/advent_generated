
class Point {
    int x, y
    Point(int x, int y) { this.x = x; this.y = y }
    @Override
    boolean equals(o) {
        if (this.is(o)) return true
        if (getClass() != o.class) return false
        Point point = (Point) o
        if (x != point.x) return false
        if (y != point.y) return false
        return true
    }
    @Override
    int hashCode() {
        int result = x
        result = 31 * result + y
        return result
    }
}

def solve() {
    def grid = new File("input.txt").readLines()
    def h = grid.size()
    def w = grid[0].size()
    Point S = null, E = null
    def walls = new boolean[h][w]
    def trackCells = []
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            def ch = grid[i][j]
            if (ch == 'S') {
                S = new Point(i, j)
            } else if (ch == 'E') {
                E = new Point(i, j)
            }
            if (ch == '#') {
                walls[i][j] = true
            } else {
                trackCells << new Point(i, j)
            }
        }
    }

    def dirs = [[1, 0], [-1, 0], [0, 1], [0, -1]]
    def isTrack = { int x, int y -> x >= 0 && x < h && y >= 0 && y < w && !walls[x][y] }

    def normalDistFrom = { Point start ->
        def dist = new int[h][w]
        for (int i = 0; i < h; i++) {
            for (int j = 0; j < w; j++) {
                dist[i][j] = -1
            }
        }
        dist[start.x][start.y] = 0
        def q = [start]
        while (q) {
            def cur = q.remove(0)
            for (def d : dirs) {
                def nx = cur.x + d[0]
                def ny = cur.y + d[1]
                if (nx < 0 || nx >= h || ny < 0 || ny >= w) continue
                if (walls[nx][ny]) continue
                if (dist[nx][ny] < 0) {
                    dist[nx][ny] = dist[cur.x][cur.y] + 1
                    q << new Point(nx, ny)
                }
            }
        }
        return dist
    }

    def distFromS = normalDistFrom(S)
    def distFromE = normalDistFrom(E)
    if (distFromS[E.x][E.y] < 0) {
        println 0
        return
    }
    def normalCost = distFromS[E.x][E.y]

    def cheats = [:]
    trackCells.each { Point startPos ->
        def sd = distFromS[startPos.x][startPos.y]
        if (sd < 0) return

        def distC = new int[h][w]
        for (int i = 0; i < h; i++) {
            for (int j = 0; j < w; j++) {
                distC[i][j] = -1
            }
        }
        distC[startPos.x][startPos.y] = 0
        def q = [startPos]

        while (q) {
            def cur = q.remove(0)
            def steps = distC[cur.x][cur.y]
            if (steps == 20) continue
            for (def d : dirs) {
                def nx = cur.x + d[0]
                def ny = cur.y + d[1]
                if (nx < 0 || nx >= h || ny < 0 || ny >= w) continue
                if (distC[nx][ny] < 0) {
                    distC[nx][ny] = steps + 1
                    q << new Point(nx, ny)
                }
            }
        }

        for (int x = 0; x < h; x++) {
            for (int y = 0; y < w; y++) {
                def s = distC[x][y]
                if (s > 0 && s <= 20 && isTrack(x, y)) {
                    def ed = distFromE[x][y]
                    if (ed < 0) continue
                    def cost = sd + s + ed
                    if (cost < normalCost) {
                        def key = [startPos.x, startPos.y, x, y]
                        def old = cheats[key]
                        if (!old || cost < old) {
                            cheats[key] = cost
                        }
                    }
                }
            }
        }
    }

    def count = cheats.values().count { normalCost - it >= 100 }
    println count
}

solve()
