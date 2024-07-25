
import java.nio.file.Files
import java.nio.file.Paths
import java.util.LinkedList
import java.util.Queue

class MazeSolver {
    static int favoriteNumber

    static void main(String[] args) {
        // Read the favorite number from input.txt
        favoriteNumber = Integer.parseInt(new String(Files.readAllBytes(Paths.get("input.txt"))).trim())
        
        // Define the target coordinates
        int targetX = 31
        int targetY = 39
        
        // Find the fewest steps to reach the target
        int steps = bfs(1, 1, targetX, targetY)
        println "Fewest number of steps required: $steps"
    }

    static int bfs(int startX, int startY, int targetX, int targetY) {
        Queue<int[]> queue = new LinkedList<>()
        Set<String> visited = new HashSet<>()
        queue.add([startX, startY])
        visited.add("$startX,$startY")
        
        int steps = 0
        
        while (!queue.isEmpty()) {
            int size = queue.size()
            for (int i = 0; i < size; i++) {
                int[] current = queue.poll()
                int x = current[0]
                int y = current[1]
                
                // Check if we reached the target
                if (x == targetX && y == targetY) {
                    return steps
                }
                
                // Explore neighbors
                for (int[] neighbor : getNeighbors(x, y)) {
                    int nx = neighbor[0]
                    int ny = neighbor[1]
                    String pos = "$nx,$ny"
                    
                    if (!visited.contains(pos) && isOpenSpace(nx, ny)) {
                        visited.add(pos)
                        queue.add(neighbor)
                    }
                }
            }
            steps++
        }
        
        return -1 // Return -1 if the target is unreachable
    }

    static List<int[]> getNeighbors(int x, int y) {
        return [[x + 1, y], [x - 1, y], [x, y + 1], [x, y - 1]].findAll { it[0] >= 0 && it[1] >= 0 }
    }

    static boolean isOpenSpace(int x, int y) {
        int value = x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber
        int onesCount = Integer.bitCount(value)
        return onesCount % 2 == 0 // Even number of 1s means open space
    }
}
