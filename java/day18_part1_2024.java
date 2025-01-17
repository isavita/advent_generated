
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedList;
import java.util.Queue;

public class RAMRun {

    public static void main(String[] args) {
        try {
            int[][] grid = readInput("input.txt");
            int minSteps = findShortestPath(grid);
            System.out.println(minSteps);
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static int[][] readInput(String filename) throws IOException {
        int[][] grid = new int[71][71]; // Initialize grid with safe cells (0)
        BufferedReader reader = new BufferedReader(new FileReader(filename));
        String line;
        int byteCount = 0;
        while ((line = reader.readLine()) != null && byteCount < 1024) {
            String[] parts = line.split(",");
            int x = Integer.parseInt(parts[0]);
            int y = Integer.parseInt(parts[1]);
            grid[y][x] = 1; // Mark cell as corrupted (1)
            byteCount++;
        }
        reader.close();
        return grid;
    }

    private static int findShortestPath(int[][] grid) {
        Queue<int[]> queue = new LinkedList<>();
        queue.offer(new int[]{0, 0, 0}); // Start at (0, 0) with 0 steps
        boolean[][] visited = new boolean[71][71];
        visited[0][0] = true;
        int[][] directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}}; // Up, Down, Left, Right

        while (!queue.isEmpty()) {
            int[] current = queue.poll();
            int row = current[0];
            int col = current[1];
            int steps = current[2];

            if (row == 70 && col == 70) {
                return steps; // Reached the exit
            }

            for (int[] dir : directions) {
                int newRow = row + dir[0];
                int newCol = col + dir[1];

                if (newRow >= 0 && newRow <= 70 && newCol >= 0 && newCol <= 70 &&
                        grid[newRow][newCol] == 0 && !visited[newRow][newCol]) {
                    queue.offer(new int[]{newRow, newCol, steps + 1});
                    visited[newRow][newCol] = true;
                }
            }
        }

        return -1; // No path found
    }
}
