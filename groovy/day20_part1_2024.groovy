
def solve() {
    def grid = new File("input.txt").readLines()
    def h = grid.size()
    def w = grid[0].size()
    def S, E
    def trackCells = []
    def walls = new boolean[h][w]

    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            def ch = grid[i][j]
            if (ch == 'S') {
                S = [x: i, y: j]
            } else if (ch == 'E') {
                E = [x: i, y: j]
            }
            if (ch == '#') {
                walls[i][j] = true
            } else {
                trackCells << [x: i, y: j]
            }
        }
    }

    def dirs = [[1, 0], [-1, 0], [0, 1], [0, -1]]

    def normalDistFrom = { start ->
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
                if (nx < 0 || nx >= h || ny < 0 || ny >= w) {
                    continue
                }
                if (walls[nx][ny]) {
                    continue
                }
                if (dist[nx][ny] == -1) {
                    dist[nx][ny] = dist[cur.x][cur.y] + 1
                    q << [x: nx, y: ny]
                }
            }
        }
        return dist
    }

    def distFromS = normalDistFrom(S)
    def distFromE = normalDistFrom(E)

    if (distFromS[E.x][E.y] == -1) {
        println 0
        return
    }

    def normalCost = distFromS[E.x][E.y]

    def isTrack = { x, y ->
        x >= 0 && x < h && y >= 0 && y < w && !walls[x][y]
    }

    def possibleCheats = 0
    for (def startPos : trackCells) {
        def sd = distFromS[startPos.x][startPos.y]
        if (sd == -1) {
            continue
        }
        for (def d1 : dirs) {
            def m1 = [x: startPos.x + d1[0], y: startPos.y + d1[1]]
            if (m1.x < 0 || m1.x >= h || m1.y < 0 || m1.y >= w) {
                continue
            }
            for (def d2 : dirs) {
                def m2 = [x: m1.x + d2[0], y: m1.y + d2[1]]
                if (m2.x < 0 || m2.x >= h || m2.y < 0 || m2.y >= w) {
                    continue
                }
                if (!isTrack(m2.x, m2.y)) {
                    continue
                }
                def ed = distFromE[m2.x][m2.y]
                if (ed == -1) {
                    continue
                }
                def newCost = sd + 2 + ed
                if (normalCost - newCost >= 100) {
                    possibleCheats++
                }
            }
        }
    }
    println possibleCheats
}

solve()
