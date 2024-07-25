
class LightGrid {
    private static final int SIZE = 100
    private char[][] grid

    LightGrid(List<String> initialConfiguration) {
        grid = new char[SIZE][SIZE]
        for (int i = 0; i < SIZE; i++) {
            grid[i] = initialConfiguration[i].toCharArray()
        }
        // Ensure corners are always on
        grid[0][0] = '#'
        grid[0][SIZE - 1] = '#'
        grid[SIZE - 1][0] = '#'
        grid[SIZE - 1][SIZE - 1] = '#'
    }

    void step() {
        char[][] newGrid = new char[SIZE][SIZE]
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                newGrid[i][j] = nextState(i, j)
            }
        }
        // Ensure corners remain on
        newGrid[0][0] = '#'
        newGrid[0][SIZE - 1] = '#'
        newGrid[SIZE - 1][0] = '#'
        newGrid[SIZE - 1][SIZE - 1] = '#'
        grid = newGrid
    }

    private char nextState(int x, int y) {
        int onNeighbors = countOnNeighbors(x, y)
        if (grid[x][y] == '#') {
            return (onNeighbors == 2 || onNeighbors == 3) ? '#' : '.'
        } else {
            return (onNeighbors == 3) ? '#' : '.'
        }
    }

    private int countOnNeighbors(int x, int y) {
        int count = 0
        for (int dx = -1; dx <= 1; dx++) {
            for (int dy = -1; dy <= 1; dy++) {
                if (dx == 0 && dy == 0) continue // Skip the current light
                int nx = x + dx
                int ny = y + dy
                if (nx >= 0 && nx < SIZE && ny >= 0 && ny < SIZE && grid[nx][ny] == '#') {
                    count++
                }
            }
        }
        return count
    }

    int countLightsOn() {
        int count = 0
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                if (grid[i][j] == '#') {
                    count++
                }
            }
        }
        return count
    }
}

def readInput(fileName) {
    new File(fileName).readLines()
}

def main() {
    def initialConfiguration = readInput("input.txt")
    LightGrid grid = new LightGrid(initialConfiguration)

    // Simulate 100 steps
    for (int step = 0; step < 100; step++) {
        grid.step()
    }

    println "Number of lights on after 100 steps: ${grid.countLightsOn()}"
}

main()
