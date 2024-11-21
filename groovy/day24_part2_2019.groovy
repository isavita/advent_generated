
class RecursiveBugs {
    static final int SIDE = 5
    static final int SQUARE = SIDE * SIDE

    static void main(String[] args) {
        def space = [0: parseInput()]

        200.times {
            space = nextGeneration(space)
        }

        def totalBugs = space.values().sum { grid ->
            grid.count { it }
        }

        println totalBugs
    }

    static Map<Integer, List<Boolean>> nextGeneration(Map<Integer, List<Boolean>> space) {
        def newSpace = [:]

        def (minLevel, maxLevel) = findLevelBounds(space)

        (minLevel - 1..maxLevel + 1).each { level ->
            newSpace[level] = new ArrayList(Collections.nCopies(SQUARE, false))

            SQUARE.times { cell ->
                if (cell == 12) return

                def neighbours = countNeighbors(space, level, cell)
                def isInfested = isInfested(space, level, cell)

                if (isInfested && neighbours != 1) {
                    newSpace[level][cell] = false
                } else if (!isInfested && (neighbours == 1 || neighbours == 2)) {
                    newSpace[level][cell] = true
                } else {
                    newSpace[level][cell] = isInfested
                }
            }
        }

        cleanSpace(newSpace)
        newSpace
    }

    static int countNeighbors(Map<Integer, List<Boolean>> space, int level, int cell) {
        int neighbours = 0
        int row = cell / SIDE
        int col = cell % SIDE

        // Edge and recursive neighbors logic
        if (row == 0) neighbours += isInfested(space, level - 1, 7) ? 1 : 0
        if (col == 0) neighbours += isInfested(space, level - 1, 11) ? 1 : 0
        if (col == 4) neighbours += isInfested(space, level - 1, 13) ? 1 : 0
        if (row == 4) neighbours += isInfested(space, level - 1, 17) ? 1 : 0

        // Recursive inner neighbors
        if (cell == 7) {
            SIDE.times { i -> 
                neighbours += isInfested(space, level + 1, i) ? 1 : 0 
            }
        }
        if (cell == 11) {
            SIDE.times { i -> 
                neighbours += isInfested(space, level + 1, 5 * i) ? 1 : 0 
            }
        }
        if (cell == 13) {
            SIDE.times { i -> 
                neighbours += isInfested(space, level + 1, 5 * i + SIDE - 1) ? 1 : 0 
            }
        }
        if (cell == 17) {
            SIDE.times { i -> 
                neighbours += isInfested(space, level + 1, (SIDE - 1) * SIDE + i) ? 1 : 0 
            }
        }

        // Regular neighbors
        if (row > 0 && cell != 17) neighbours += isInfested(space, level, cell - SIDE) ? 1 : 0
        if (col > 0 && cell != 13) neighbours += isInfested(space, level, cell - 1) ? 1 : 0
        if (col < SIDE - 1 && cell != 11) neighbours += isInfested(space, level, cell + 1) ? 1 : 0
        if (row < SIDE - 1 && cell != 7) neighbours += isInfested(space, level, cell + SIDE) ? 1 : 0

        neighbours
    }

    static List<Boolean> parseInput() {
        def input = new File('input.txt').readLines().collectMany { line ->
            line.toCharArray().collect { it == '#' }
        }
        input
    }

    static boolean isInfested(Map<Integer, List<Boolean>> space, int level, int cell) {
        space[level]?.getAt(cell) ?: false
    }

    static void cleanSpace(Map<Integer, List<Boolean>> space) {
        def (min, max) = findLevelBounds(space)

        def minCount = space[min].count { it }
        def maxCount = space[max].count { it }

        if (minCount == 0) space.remove(min)
        if (maxCount == 0) space.remove(max)
    }

    static List<Integer> findLevelBounds(Map<Integer, List<Boolean>> space) {
        def levels = space.keySet()
        [levels.min(), levels.max()]
    }
}
