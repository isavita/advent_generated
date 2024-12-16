
def solve() {
    def map = new File('input.txt').readLines().collect { it.toList().collect { it.toInteger() } }
    def rows = map.size()
    def cols = map[0].size()
    def trailheads = []
    
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (map[r][c] == 0) {
                trailheads << [r, c]
            }
        }
    }

    def totalScore = 0
    trailheads.each { trailhead ->
        def score = 0
        def visited = new HashSet()
        def queue = [[trailhead[0], trailhead[1]]]
        
        while (!queue.isEmpty()) {
            def current = queue.remove(0)
            def r = current[0]
            def c = current[1]
            
            if (visited.contains([r, c])) {
                continue
            }
            visited.add([r, c])
            
            if (map[r][c] == 9) {
                score++
            }
            
            def neighbors = [[r - 1, c], [r + 1, c], [r, c - 1], [r, c + 1]]
            neighbors.each { neighbor ->
                def nr = neighbor[0]
                def nc = neighbor[1]
                if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && map[nr][nc] == map[r][c] + 1) {
                    queue << [nr, nc]
                }
            }
        }
        totalScore += score
    }
    println totalScore
}

solve()
