
def solvePart1(List<String> initialGrid, int bursts) {
    def grid = [:]
    int rows = initialGrid.size()
    int cols = initialGrid[0].size()
    int rowOffset = rows / 2
    int colOffset = cols / 2

    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            grid[[i - rowOffset, j - colOffset]] = initialGrid[i][j] == '#'
        }
    }

    int infectedCount = 0
    int currentRow = 0
    int currentCol = 0
    int direction = 0 // 0: up, 1: right, 2: down, 3: left

    for (int i = 0; i < bursts; i++) {
        def infected = grid.get([currentRow, currentCol], false)

        // Turn
        if (infected) {
            direction = (direction + 1) % 4
        } else {
            direction = (direction + 3) % 4
        }

        // Infect/Clean
        if (infected) {
            grid[[currentRow, currentCol]] = false
        } else {
            grid[[currentRow, currentCol]] = true
            infectedCount++
        }

        // Move
        switch (direction) {
            case 0: currentRow--
                     break
            case 1: currentCol++
                     break
            case 2: currentRow++
                     break
            case 3: currentCol--
                     break
        }
    }

    return infectedCount
}


def solvePart2(List<String> initialGrid, int bursts) {
    def grid = [:]
    int rows = initialGrid.size()
    int cols = initialGrid[0].size()
    int rowOffset = rows / 2
    int colOffset = cols / 2

    // States: 0: Clean, 1: Weakened, 2: Infected, 3: Flagged
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            grid[[i - rowOffset, j - colOffset]] = initialGrid[i][j] == '#' ? 2 : 0  // Infected : Clean
        }
    }

    int infectedCount = 0
    int currentRow = 0
    int currentCol = 0
    int direction = 0 // 0: up, 1: right, 2: down, 3: left

    for (int i = 0; i < bursts; i++) {
        def state = grid.get([currentRow, currentCol], 0) // default to Clean

        // Turn
        switch (state) {
            case 0: direction = (direction + 3) % 4; break // Clean: turn left
            case 2: direction = (direction + 1) % 4; break // Infected: turn right
            case 3: direction = (direction + 2) % 4; break // Flagged: reverse direction
        }

        // Update State
        switch (state) {
            case 0: grid[[currentRow, currentCol]] = 1; break // Clean -> Weakened
            case 1: grid[[currentRow, currentCol]] = 2; infectedCount++; break // Weakened -> Infected
            case 2: grid[[currentRow, currentCol]] = 3; break // Infected -> Flagged
            case 3: grid[[currentRow, currentCol]] = 0; break // Flagged -> Clean
        }

        // Move
        switch (direction) {
            case 0: currentRow--
                     break
            case 1: currentCol++
                     break
            case 2: currentRow++
                     break
            case 3: currentCol--
                     break
        }
    }

    return infectedCount
}


def main() {
    def input = new File("input.txt").readLines()

    println "Part 1: ${solvePart1(input, 10000)}"
    println "Part 2: ${solvePart2(input, 10000000)}"
}

main()
