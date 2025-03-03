
class HillClimbing {

    static void main(String[] args) {
        def input = new File('input.txt').readLines()
        def grid = input.collect { it.toList() }

        def start = findStart(grid)
        def end = findEnd(grid)

        grid[start.row][start.col] = 'a'
        grid[end.row][end.col] = 'z'

        def steps = shortestPath(grid, start, end)

        println steps
    }

    static Point findStart(List<List<Character>> grid) {
        for (int i = 0; i < grid.size(); i++) {
            for (int j = 0; j < grid[0].size(); j++) {
                if (grid[i][j] == 'S') {
                    return new Point(i, j)
                }
            }
        }
        return null
    }

    static Point findEnd(List<List<Character>> grid) {
        for (int i = 0; i < grid.size(); i++) {
            for (int j = 0; j < grid[0].size(); j++) {
                if (grid[i][j] == 'E') {
                    return new Point(i, j)
                }
            }
        }
        return null
    }

    static int shortestPath(List<List<Character>> grid, Point start, Point end) {
        int rows = grid.size()
        int cols = grid[0].size()

        def queue = new LinkedList<Node>()
        queue.add(new Node(start, 0))

        def visited = new HashSet<Point>()
        visited.add(start)

        while (!queue.isEmpty()) {
            Node current = queue.remove()
            Point point = current.point
            int steps = current.steps

            if (point == end) {
                return steps
            }

            def neighbors = getNeighbors(grid, point)

            for (Point neighbor : neighbors) {
                if (!visited.contains(neighbor) && isValidMove(grid, point, neighbor)) {
                    queue.add(new Node(neighbor, steps + 1))
                    visited.add(neighbor)
                }
            }
        }

        return -1 // No path found
    }

    static List<Point> getNeighbors(List<List<Character>> grid, Point point) {
        int rows = grid.size()
        int cols = grid[0].size()

        def neighbors = []

        int row = point.row
        int col = point.col

        if (row > 0) {
            neighbors << new Point(row - 1, col)
        }
        if (row < rows - 1) {
            neighbors << new Point(row + 1, col)
        }
        if (col > 0) {
            neighbors << new Point(row, col - 1)
        }
        if (col < cols - 1) {
            neighbors << new Point(row, col + 1)
        }

        return neighbors
    }

    static boolean isValidMove(List<List<Character>> grid, Point current, Point next) {
        int currentRow = current.row
        int currentCol = current.col
        int nextRow = next.row
        int nextCol = next.col

        char currentHeight = grid[currentRow][currentCol]
        char nextHeight = grid[nextRow][nextCol]

        return nextHeight.charValue() <= currentHeight.charValue() + 1
    }

    static class Point {
        int row
        int col

        Point(int row, int col) {
            this.row = row
            this.col = col
        }

        @Override
        boolean equals(Object o) {
            if (this.is(o)) return true
            if (getClass() != o.getClass()) return false

            Point point = (Point) o

            if (row != point.row) return false
            if (col != point.col) return false

            return true
        }

        @Override
        int hashCode() {
            int result = row
            result = 31 * result + col
            return result
        }

        @Override
        String toString() {
            return "Point{" +
                    "row=" + row +
                    ", col=" + col +
                    '}';
        }
    }

    static class Node {
        Point point
        int steps

        Node(Point point, int steps) {
            this.point = point
            this.steps = steps
        }
    }
}
