
import java.util.PriorityQueue

class Chiton {

    static void main(String[] args) {
        def grid = new File('input.txt').readLines().collect { it.toList().collect { it.toInteger() } }
        
        println "Part 1: ${findLowestRiskPath(grid)}"
        
        def fullGrid = expandGrid(grid, 5)
        println "Part 2: ${findLowestRiskPath(fullGrid)}"
    }

    static int findLowestRiskPath(List<List<Integer>> grid) {
        int rows = grid.size()
        int cols = grid[0].size()

        def dist = [:]
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                dist[[i, j]] = Integer.MAX_VALUE
            }
        }
        dist[[0, 0]] = 0

        PriorityQueue<Node> pq = new PriorityQueue<>()
        pq.add(new Node(0, 0, 0))

        while (!pq.isEmpty()) {
            Node u = pq.poll()
            int row = u.row
            int col = u.col
            int d = u.dist

            if (d > dist[[row, col]]) {
                continue
            }

            def neighbors = [[row - 1, col], [row + 1, col], [row, col - 1], [row, col + 1]]
            for (def neighbor : neighbors) {
                int newRow = neighbor[0]
                int newCol = neighbor[1]

                if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols) {
                    int weight = grid[newRow][newCol]
                    if (dist[[row, col]] + weight < dist[[newRow, newCol]]) {
                        dist[[newRow, newCol]] = dist[[row, col]] + weight
                        pq.add(new Node(newRow, newCol, dist[[newRow, newCol]]))
                    }
                }
            }
        }

        return dist[[rows - 1, cols - 1]]
    }
    
    static List<List<Integer>> expandGrid(List<List<Integer>> grid, int times) {
        int rows = grid.size()
        int cols = grid[0].size()
        
        def expandedGrid = []
        for (int i = 0; i < rows * times; i++) {
            expandedGrid.add([])
            for (int j = 0; j < cols * times; j++) {
                int originalRow = i % rows
                int originalCol = j % cols
                int tileRow = i / rows
                int tileCol = j / cols
                
                int newValue = grid[originalRow][originalCol] + tileRow + tileCol
                while (newValue > 9) {
                    newValue -= 9
                }
                expandedGrid[i].add(newValue)
            }
        }
        
        return expandedGrid
    }

    static class Node implements Comparable<Node> {
        int row
        int col
        int dist

        Node(int row, int col, int dist) {
            this.row = row
            this.col = col
            this.dist = dist
        }

        @Override
        int compareTo(Node other) {
            return Integer.compare(this.dist, other.dist)
        }
    }
}
