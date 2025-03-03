
import java.util.PriorityQueue

class Day17 {

    static final int PART1_MIN_STEPS = 0
    static final int PART1_MAX_STEPS = 3
    static final int PART2_MIN_STEPS = 4
    static final int PART2_MAX_STEPS = 10

    static class State implements Comparable<State> {
        int row
        int col
        int dirRow
        int dirCol
        int steps
        int heatLoss

        State(int row, int col, int dirRow, int dirCol, int steps, int heatLoss) {
            this.row = row
            this.col = col
            this.dirRow = dirRow
            this.dirCol = dirCol
            this.steps = steps
            this.heatLoss = heatLoss
        }

        @Override
        int compareTo(State other) {
            return this.heatLoss - other.heatLoss
        }

        @Override
        boolean equals(Object o) {
            if (this.is(o)) return true
            if (getClass() != o.class) return false
            State state = (State) o
            return row == state.row &&
                    col == state.col &&
                    dirRow == state.dirRow &&
                    dirCol == state.dirCol &&
                    steps == state.steps
        }

        @Override
        int hashCode() {
            return Objects.hash(row, col, dirRow, dirCol, steps)
        }
    }


    static int solve(List<List<Integer>> grid, int minSteps, int maxSteps) {
        int rows = grid.size()
        int cols = grid[0].size()

        PriorityQueue<State> pq = new PriorityQueue<>()
        pq.offer(new State(0, 0, 0, 1, 0, 0)) // Start moving right
        pq.offer(new State(0, 0, 1, 0, 0, 0)) // Start moving down

        Set<State> visited = new HashSet<>()
        Map<State, Integer> distances = new HashMap<>()

        while (!pq.isEmpty()) {
            State current = pq.poll()

            if (current.row == rows - 1 && current.col == cols - 1 && current.steps >= minSteps) {
                return current.heatLoss
            }

            if (visited.contains(current)) {
                continue
            }
            visited.add(current)
            
            // Try turning
            if (current.steps >= minSteps) {
                int[][] turns = [[current.dirCol, current.dirRow], [-current.dirCol, -current.dirRow]]
                for (int[] turn : turns) {
                    int newDirRow = turn[0]
                    int newDirCol = turn[1]
                    int newRow = current.row + newDirRow
                    int newCol = current.col + newDirCol

                    if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols) {
                        int newHeatLoss = current.heatLoss + grid[newRow][newCol]
                        State newState = new State(newRow, newCol, newDirRow, newDirCol, 1, newHeatLoss)

                        if (!visited.contains(newState)) {
                            pq.offer(newState)
                        }
                    }
                }
            }

            // Try continuing straight
            if (current.steps < maxSteps) {
                int newRow = current.row + current.dirRow
                int newCol = current.col + current.dirCol
                if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols) {
                    int newHeatLoss = current.heatLoss + grid[newRow][newCol]
                    State newState = new State(newRow, newCol, current.dirRow, current.dirCol, current.steps + 1, newHeatLoss)
                    if (!visited.contains(newState)) {
                        pq.offer(newState)
                    }

                }
            }
        }
        return -1 // Should not happen if there's always a path
    }


    static void main(String[] args) {
        List<List<Integer>> grid = []
        new File("input.txt").eachLine { line ->
            grid.add(line.collect { it.toInteger() })
        }

        println "Part 1: ${solve(grid, PART1_MIN_STEPS, PART1_MAX_STEPS)}"
        println "Part 2: ${solve(grid, PART2_MIN_STEPS, PART2_MAX_STEPS)}"
    }
}
