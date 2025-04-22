
import java.util.*

def solve() {
    def lines = new File('input.txt').readLines()
    def grid = lines.collect { it.toList() }
    def height = grid.size()
    def width = grid[0].size()

    def start = [0, 0]
    for (int j = 0; j < width; j++) {
        if (grid[0][j] == '.') {
            start = [0, j]
            break
        }
    }

    def end = [0, 0]
    for (int j = 0; j < width; j++) {
        if (grid[height - 1][j] == '.') {
            end = [height - 1, j]
            break
        }
    }

    def blizzards = []
    for (int i = 0; i < height; i++) {
        for (int j = 0; j < width; j++) {
            def c = grid[i][j]
            if (c == '>' || c == '<' || c == '^' || c == 'v') {
                blizzards << [[i, j], c]
            }
        }
    }

    def getBlizzardsAtTime = { int time ->
        def blizzardPositions = new HashSet<List<Integer>>()
        blizzards.each { blizzard ->
            def pos = blizzard[0]
            def dir = blizzard[1]
            def newRow = pos[0]
            def newCol = pos[1]

            if (dir == '>') {
                newCol = (pos[1] - 1 + time) % (width - 2) + 1
            } else if (dir == '<') {
                newCol = (pos[1] - 1 - time % (width - 2) + (width - 2)) % (width - 2) + 1
            } else if (dir == '^') {
                newRow = (pos[0] - 1 - time % (height - 2) + (height - 2)) % (height - 2) + 1
            } else if (dir == 'v') {
                newRow = (pos[0] - 1 + time) % (height - 2) + 1
            }

            blizzardPositions.add([newRow, newCol])
        }
        return blizzardPositions
    }


    def bfs = { startPos, endPos, startTime ->
        def queue = new LinkedList<List<Integer>>()
        queue.add([startPos[0], startPos[1], startTime])
        def visited = new HashSet<List<Integer>>()
        visited.add([startPos[0], startPos[1], startTime])

        while (!queue.isEmpty()) {
            def current = queue.removeFirst()
            def row = current[0]
            def col = current[1]
            def time = current[2]

            if (row == endPos[0] && col == endPos[1]) {
                return time
            }

            def nextTime = time + 1
            def nextBlizzards = getBlizzardsAtTime(nextTime)

            def moves = [[0, 0], [0, 1], [0, -1], [1, 0], [-1, 0]] // Stay, Right, Left, Down, Up

            moves.each { move ->
                def nextRow = row + move[0]
                def nextCol = col + move[1]

                if (nextRow >= 0 && nextRow < height && nextCol >= 0 && nextCol < width &&
                        grid[nextRow][nextCol] != '#' && !nextBlizzards.contains([nextRow, nextCol])) {
                    if (!visited.contains([nextRow, nextCol, nextTime])) {
                        queue.add([nextRow, nextCol, nextTime])
                        visited.add([nextRow, nextCol, nextTime])
                    }
                }
            }
        }
        return -1 // Should not happen if a path exists
    }

    def trip1 = bfs(start, end, 0)
    println "Trip 1: " + trip1
    def trip2 = bfs(end, start, trip1)
    println "Trip 2: " + trip2
    def trip3 = bfs(start, end, trip2)
    println "Trip 3: " + trip3

    println "Total Time: " + trip3
}

solve()
