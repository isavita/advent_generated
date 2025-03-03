
import java.util.PriorityQueue

class ManyWorlds {

    static void main(String[] args) {
        File inputFile = new File("input.txt")
        List<String> map = inputFile.readLines()

        int startRow = -1
        int startCol = -1
        Map<Character, Coordinate> keyPositions = [:]
        Set<Character> allKeys = new HashSet<>()

        for (int i = 0; i < map.size(); i++) {
            for (int j = 0; j < map[i].length(); j++) {
                char c = map[i][j]
                if (c == '@') {
                    startRow = i
                    startCol = j
                } else if (c >= 'a' && c <= 'z') {
                    keyPositions[c] = new Coordinate(i, j)
                    allKeys.add(c)
                }
            }
        }

        println findShortestPath(map, startRow, startCol, keyPositions, allKeys)
    }

    static int findShortestPath(List<String> map, int startRow, int startCol, Map<Character, Coordinate> keyPositions, Set<Character> allKeys) {
        State initialState = new State(startRow, startCol, new HashSet<>(), 0)
        PriorityQueue<State> queue = new PriorityQueue<>(Comparator.comparingInt({ it.distance }))
        queue.add(initialState)

        Set<State> visited = new HashSet<>()
        visited.add(initialState)

        while (!queue.isEmpty()) {
            State currentState = queue.poll()
            int row = currentState.row
            int col = currentState.col
            Set<Character> collectedKeys = currentState.collectedKeys
            int distance = currentState.distance

            if (collectedKeys.size() == allKeys.size()) {
                return distance
            }

            List<Coordinate> neighbors = getNeighbors(map, row, col)
            for (Coordinate neighbor : neighbors) {
                int newRow = neighbor.row
                int newCol = neighbor.col
                char cell = map[newRow][newCol]

                Set<Character> newCollectedKeys = new HashSet<>(collectedKeys)

                if (cell >= 'a' && cell <= 'z') {
                    newCollectedKeys.add(cell)
                } else if (cell >= 'A' && cell <= 'Z') {
                    char lowerCase = Character.toLowerCase(cell)
                    if (!collectedKeys.contains(lowerCase)) {
                        continue // Cannot pass through locked door
                    }
                }

                State newState = new State(newRow, newCol, newCollectedKeys, distance + 1)
                if (!visited.contains(newState)) {
                    queue.add(newState)
                    visited.add(newState)
                }
            }
        }

        return -1 // Should never happen if a path exists
    }

    static List<Coordinate> getNeighbors(List<String> map, int row, int col) {
        List<Coordinate> neighbors = []
        int[][] directions = [[0, 1], [0, -1], [1, 0], [-1, 0]]

        for (int[] direction : directions) {
            int newRow = row + direction[0]
            int newCol = col + direction[1]

            if (newRow >= 0 && newRow < map.size() && newCol >= 0 && newCol < map[0].length() && map[newRow][newCol] != '#') {
                neighbors.add(new Coordinate(newRow, newCol))
            }
        }

        return neighbors
    }

    static class Coordinate {
        int row
        int col

        Coordinate(int row, int col) {
            this.row = row
            this.col = col
        }

        @Override
        boolean equals(Object o) {
            if (this.is(o)) return true
            if (getClass() != o.getClass()) return false
            Coordinate that = (Coordinate) o
            return row == that.row && col == that.col
        }

        @Override
        int hashCode() {
            return Objects.hash(row, col)
        }
    }

    static class State {
        int row
        int col
        Set<Character> collectedKeys
        int distance

        State(int row, int col, Set<Character> collectedKeys, int distance) {
            this.row = row
            this.col = col
            this.collectedKeys = collectedKeys
            this.distance = distance
        }

        @Override
        boolean equals(Object o) {
            if (this.is(o)) return true
            if (getClass() != o.getClass()) return false
            State state = (State) o
            return row == state.row && col == state.col && Objects.equals(collectedKeys, state.collectedKeys)
        }

        @Override
        int hashCode() {
            return Objects.hash(row, col, collectedKeys)
        }
    }
}
