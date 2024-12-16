
def calculateLoad(platform) {
    int rows = platform.size()
    int cols = platform[0].size()
    int totalLoad = 0

    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (platform[r][c] == 'O') {
                totalLoad += (rows - r)
            }
        }
    }
    return totalLoad
}

def tiltNorth(platform) {
    int rows = platform.size()
    int cols = platform[0].size()
    def newPlatform = platform.collect { it.clone() }

    for (int c = 0; c < cols; c++) {
        int nextOpenRow = 0
        for (int r = 0; r < rows; r++) {
            if (newPlatform[r][c] == 'O') {
                newPlatform[r][c] = '.'
                newPlatform[nextOpenRow][c] = 'O'
                nextOpenRow++
            } else if (newPlatform[r][c] == '#') {
                nextOpenRow = r + 1
            }
        }
    }
    return newPlatform
}

def solve() {
    def platform = new File('input.txt').readLines().collect { it.toList() }
    def tiltedPlatform = tiltNorth(platform)
    println calculateLoad(tiltedPlatform)
}

solve()
