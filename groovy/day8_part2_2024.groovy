
def gcd(a, b) {
    b == 0 ? Math.abs(a) : gcd(b, a % b)
}

def solve() {
    def grid = new File("input.txt").readLines()
    def h = grid.size()
    def w = grid[0].size()
    def antennas = [:]

    for (y in 0..<h) {
        for (x in 0..<w) {
            def c = grid[y][x]
            if (c != '.') {
                antennas.computeIfAbsent(c, { [] }).add([y, x])
            }
        }
    }

    def linesPerFreq = [:]
    antennas.each { f, coords ->
        def lines = [] as Set
        def n = coords.size()
        for (i in 0..<n) {
            for (j in (i + 1)..<n) {
                def dy = coords[j][0] - coords[i][0]
                def dx = coords[j][1] - coords[i][1]
                def g = gcd(dy, dx)
                def sy = dy / g
                def sx = dx / g
                if (sx < 0 || (sx == 0 && sy < 0)) {
                    sx = -sx
                    sy = -sy
                }
                def c = sy * coords[i][1] - sx * coords[i][0]
                lines << [sx, sy, c]
            }
        }
        linesPerFreq[f] = lines
    }

    def antinodes = [] as Set
    linesPerFreq.values().each { lines ->
        lines.each { sx, sy, c ->
            if (sx == 0 && sy == 0) return
            if (sy == 0) {
                if (c % sx == 0) {
                    def y = -c / sx
                    if (y >= 0 && y < h) {
                        antinodes.addAll((0..<w).collect { [y, it] })
                    }
                }
            } else if (sx == 0) {
                if (c % sy == 0) {
                    def x = c / sy
                    if (x >= 0 && x < w) {
                        antinodes.addAll((0..<h).collect { [it, x] })
                    }
                }
            } else {
                for (y in 0..<h) {
                    def val = c + sx * y
                    if (val % sy == 0) {
                        def x = val / sy
                        if (x >= 0 && x < w) {
                            antinodes << [y, x]
                        }
                    }
                }
            }
        }
    }
    println antinodes.size()
}

solve()
