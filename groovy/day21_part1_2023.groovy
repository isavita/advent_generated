
import java.nio.file.Files
import java.nio.file.Paths
import java.util.LinkedList

class StepCounter {
    static void main(String[] args) {
        def inputFile = 'input.txt'
        def lines = Files.readAllLines(Paths.get(inputFile))
        def grid = lines.collect { it.toCharArray() }
        
        int rows = grid.size()
        int cols = grid[0].length
        int startRow = -1, startCol = -1
        
        // Find the starting position (S)
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                if (grid[r][c] == 'S') {
                    startRow = r
                    startCol = c
                    break
                }
            }
            if (startRow != -1) break
        }
        
        // BFS to find reachable garden plots in exactly 64 steps
        def directions = [[-1, 0], [1, 0], [0, -1], [0, 1]] // Up, Down, Left, Right
        def queue = new LinkedList<>()
        queue.add([startRow, startCol, 0]) // [row, col, steps]
        def visited = new HashSet<>()
        visited.add("${startRow},${startCol},0")
        def reachablePlots = new HashSet<>()
        
        while (!queue.isEmpty()) {
            def (row, col, steps) = queue.poll()
            
            // If we reached exactly 64 steps, record the position if it's a garden plot
            if (steps == 64) {
                reachablePlots.add("${row},${col}")
            }
            
            // If we have taken 64 steps, we cannot take more
            if (steps >= 64) continue
            
            // Explore the four possible directions
            for (def dir : directions) {
                int newRow = row + dir[0]
                int newCol = col + dir[1]
                
                // Check if the new position is within bounds and is a garden plot
                if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols && 
                    (grid[newRow][newCol] == '.' || grid[newRow][newCol] == 'S')) {
                    
                    // Check if we have already visited this position with the same number of steps
                    if (!visited.contains("${newRow},${newCol},${steps + 1}")) {
                        visited.add("${newRow},${newCol},${steps + 1}")
                        queue.add([newRow, newCol, steps + 1])
                    }
                }
            }
        }
        
        // Output the number of unique reachable garden plots in exactly 64 steps
        println reachablePlots.size()
    }
}
