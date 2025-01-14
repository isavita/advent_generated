
def solve() {
    def map = new File('input.txt').readLines().collect { it.toList().collect { it.toInteger() } }
    def rows = map.size()
    def cols = map[0].size()

    def is_valid = { r, c -> r >= 0 && r < rows && c >= 0 && c < cols }
    def get_neighbors = { r, c ->
        [[r - 1, c], [r + 1, c], [r, c - 1], [r, c + 1]]
            .findAll { nr, nc -> is_valid(nr, nc) }
    }

    def calculate_score = { start_r, start_c ->
        def visited = new HashSet()
        def queue = [[start_r, start_c]]
        def score = 0

        while (queue) {
            def (r, c) = queue.remove(0)
            if (!visited.contains([r, c])) {
                visited.add([r, c])
                if (map[r][c] == 9) {
                    score++
                }
                get_neighbors(r, c).each { nr, nc ->
                    if (map[nr][nc] == map[r][c] + 1) {
                        queue.add([nr, nc])
                    }
                }
            }
        }
        return score
    }

    def calculate_rating = { start_r, start_c ->
        def count = 0
        def paths = [[start_r, start_c]]

        while (paths) {
            def (r, c) = paths.remove(0)
            if (map[r][c] == 9) {
                count++
            } else {
                get_neighbors(r, c).each { nr, nc ->
                    if (map[nr][nc] == map[r][c] + 1) {
                        paths.add([nr, nc])
                    }
                }
            }
        }
        return count
    }

    def total_score = 0
    def total_rating = 0
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (map[r][c] == 0) {
                total_score += calculate_score(r, c)
                total_rating += calculate_rating(r, c)
            }
        }
    }

    println "Part 1: Sum of trailhead scores: ${total_score}"
    println "Part 2: Sum of trailhead ratings: ${total_rating}"
}

solve()
