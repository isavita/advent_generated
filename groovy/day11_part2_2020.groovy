def directions = [
    [-1, -1], [0, -1], [1, -1],
    [-1, 0], [1, 0],
    [-1, 1], [0, 1], [1, 1]
]

def file = new File("input.txt")
def seatingArea = file.collect { it.toCharArray() }

def stabilized = false
while (!stabilized) {
    def newSeatingArea = seatingArea.collect { it.clone() }
    stabilized = true
    for (int i = 0; i < seatingArea.size(); i++) {
        for (int j = 0; j < seatingArea[i].size(); j++) {
            switch (seatingArea[i][j]) {
                case 'L':
                    if (countVisibleOccupied(seatingArea, i, j, directions) == 0) {
                        newSeatingArea[i][j] = '#'
                        stabilized = false
                    }
                    break
                case '#':
                    if (countVisibleOccupied(seatingArea, i, j, directions) >= 5) {
                        newSeatingArea[i][j] = 'L'
                        stabilized = false
                    }
                    break
            }
        }
    }
    seatingArea = newSeatingArea
}

println countOccupiedSeats(seatingArea)

def countVisibleOccupied(seatingArea, row, col, directions) {
    def count = 0
    for (def dir : directions) {
        def r = row + dir[1]
        def c = col + dir[0]
        while (r >= 0 && r < seatingArea.size() && c >= 0 && c < seatingArea[0].size()) {
            if (seatingArea[r][c] == 'L') break
            if (seatingArea[r][c] == '#') {
                count++
                break
            }
            r += dir[1]
            c += dir[0]
        }
    }
    count
}

def countOccupiedSeats(seatingArea) {
    def count = 0
    for (def row : seatingArea) {
        for (def seat : row) {
            if (seat == '#') count++
        }
    }
    count
}