
def solve() {
    def input = new File("input.txt").readLines()
    def grid = new boolean[71][71]
    def bytes = input.take(1024).collect { it.split(',').collect { it.toInteger() } }

    bytes.each { x, y ->
        grid[y][x] = true
    }

    def queue = [[0, 0, 0]] // [row, col, steps]
    def visited = new HashSet()
    visited.add("0,0")

    while (!queue.isEmpty()) {
        def (row, col, steps) = queue.remove(0)

        if (row == 70 && col == 70) {
            println steps
            return
        }

        def neighbors = [[row - 1, col], [row + 1, col], [row, col - 1], [row, col + 1]]
        neighbors.each { nr, nc ->
            if (nr >= 0 && nr <= 70 && nc >= 0 && nc <= 70 && !grid[nr][nc] && !visited.contains("$nr,$nc")) {
                queue.add([nr, nc, steps + 1])
                visited.add("$nr,$nc")
            }
        }
    }
}

solve()
