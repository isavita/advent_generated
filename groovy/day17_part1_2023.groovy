
import java.util.PriorityQueue

class Crucible {
    int row
    int col
    int dirRow
    int dirCol
    int steps
    int heatLoss

    Crucible(int row, int col, int dirRow, int dirCol, int steps, int heatLoss) {
        this.row = row
        this.col = col
        this.dirRow = dirRow
        this.dirCol = dirCol
        this.steps = steps
        this.heatLoss = heatLoss
    }

    @Override
    boolean equals(o) {
        if (this.is(o)) return true
        if (getClass() != o.class) return false
        Crucible crucible = (Crucible) o
        if (row != crucible.row) return false
        if (col != crucible.col) return false
        if (dirRow != crucible.dirRow) return false
        if (dirCol != crucible.dirCol) return false
        if (steps != crucible.steps) return false
        return true
    }

    @Override
    int hashCode() {
        int result = row
        result = 31 * result + col
        result = 31 * result + dirRow
        result = 31 * result + dirCol
        result = 31 * result + steps
        return result
    }
}

def solve() {
    def grid = new File("input.txt").readLines().collect { it.toList().collect { it.toInteger() } }
    def rows = grid.size()
    def cols = grid[0].size()

    def start = new Crucible(0, 0, 0, 0, 0, 0)
    def visited = new HashSet<Crucible>()
    def queue = new PriorityQueue<Crucible>({ a, b -> a.heatLoss <=> b.heatLoss })
    queue.add(start)

    def directions = [[0, 1], [1, 0], [0, -1], [-1, 0]] // Right, Down, Left, Up

    while (!queue.isEmpty()) {
        def current = queue.poll()

        if (current.row == rows - 1 && current.col == cols - 1) {
            println current.heatLoss
            return
        }

        if (visited.contains(current)) {
            continue
        }
        visited.add(current)

        for (int i = 0; i < 4; i++) {
            def newDirRow = directions[i][0]
            def newDirCol = directions[i][1]

            // Cannot reverse direction
            if (newDirRow == -current.dirRow && newDirCol == -current.dirCol) {
                continue
            }

            int newSteps = (newDirRow == current.dirRow && newDirCol == current.dirCol) ? current.steps + 1 : 1

            // Cannot move more than 3 steps in the same direction
            if (newSteps > 3) {
                continue
            }

            def newRow = current.row + newDirRow
            def newCol = current.col + newDirCol

            if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols) {
                def newHeatLoss = current.heatLoss + grid[newRow][newCol]
                def next = new Crucible(newRow, newCol, newDirRow, newDirCol, newSteps, newHeatLoss)
                queue.add(next)
            }
        }
    }
}

solve()
