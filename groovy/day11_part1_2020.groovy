
def seatingArea = new File("input.txt").readLines().collect { it as char[] }

def simulateSeating(seatingArea) {
    def rows = seatingArea.size()
    def cols = seatingArea[0].size()
    def newSeatingArea = seatingArea.collect { it.clone() }
    def stabilized = true

    for (def i = 0; i < rows; i++) {
        for (def j = 0; j < cols; j++) {
            switch (seatingArea[i][j]) {
                case 'L':
                    if (countAdjacentOccupied(seatingArea, i, j) == 0) {
                        newSeatingArea[i][j] = '#'
                        stabilized = false
                    }
                    break
                case '#':
                    if (countAdjacentOccupied(seatingArea, i, j) >= 4) {
                        newSeatingArea[i][j] = 'L'
                        stabilized = false
                    }
                    break
            }
        }
    }

    return [newSeatingArea, stabilized]
}

def countAdjacentOccupied(seatingArea, row, col) {
    def count = 0
    for (def i = row - 1; i <= row + 1; i++) {
        for (def j = col - 1; j <= col + 1; j++) {
            if (i == row && j == col) {
                continue
            }
            if (i >= 0 && i < seatingArea.size() && j >= 0 && j < seatingArea[0].size()) {
                if (seatingArea[i][j] == '#') {
                    count++
                }
            }
        }
    }
    return count
}

def countOccupiedSeats(seatingArea) {
    def count = 0
    seatingArea.each { row ->
        row.each { seat ->
            if (seat == '#') {
                count++
            }
        }
    }
    return count
}

def seatingAreaCopy = seatingArea.collect { it.clone() }
def stabilized = false
while (!stabilized) {
    def result = simulateSeating(seatingAreaCopy)
    seatingAreaCopy = result[0]
    stabilized = result[1]
}

println countOccupiedSeats(seatingAreaCopy)
