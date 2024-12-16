
def grid = new File('input.txt').readLines()
def h = grid.size()
def w = grid[0].size()
def antennas = [:]
grid.eachWithIndex { row, y ->
    row.eachWithIndex { c, x ->
        if (c != '.') {
            antennas[c] = (antennas[c] ?: []) << [y, x]
        }
    }
}

def antinodes = [:]
antennas.values().each { coords ->
    def n = coords.size()
    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {
            def A = coords[i]
            def B = coords[j]
            def P1 = [2 * A[0] - B[0], 2 * A[1] - B[1]]
            def P2 = [2 * B[0] - A[0], 2 * B[1] - A[1]]
            if (P1[0] >= 0 && P1[0] < h && P1[1] >= 0 && P1[1] < w) {
                antinodes[P1] = true
            }
            if (P2[0] >= 0 && P2[0] < h && P2[1] >= 0 && P2[1] < w) {
                antinodes[P2] = true
            }
        }
    }
}

println antinodes.size()
