
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.PriorityQueue;

public class Chiton {

    public static void main(String[] args) {
        try {
            int[][] grid = readInput("input.txt");
            System.out.println("Lowest total risk (Part 1): " + findLowestRiskPath(grid));

            int[][] expandedGrid = expandGrid(grid, 5);
            System.out.println("Lowest total risk (Part 2): " + findLowestRiskPath(expandedGrid));
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static int[][] readInput(String filename) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(filename));
        return reader.lines()
                .map(line -> line.chars().map(Character::getNumericValue).toArray())
                .toArray(int[][]::new);
    }

    private static int findLowestRiskPath(int[][] grid) {
        int rows = grid.length;
        int cols = grid[0].length;
        int[][] dist = new int[rows][cols];
        for (int[] row : dist) {
            Arrays.fill(row, Integer.MAX_VALUE);
        }
        dist[0][0] = 0;

        PriorityQueue<int[]> pq = new PriorityQueue<>((a, b) -> a[2] - b[2]);
        pq.offer(new int[]{0, 0, 0});

        int[][] dirs = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

        while (!pq.isEmpty()) {
            int[] curr = pq.poll();
            int row = curr[0];
            int col = curr[1];
            int cost = curr[2];

            if (row == rows - 1 && col == cols - 1) {
                return cost;
            }

            if (cost > dist[row][col]) {
                continue;
            }

            for (int[] dir : dirs) {
                int newRow = row + dir[0];
                int newCol = col + dir[1];

                if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols) {
                    int newCost = cost + grid[newRow][newCol];
                    if (newCost < dist[newRow][newCol]) {
                        dist[newRow][newCol] = newCost;
                        pq.offer(new int[]{newRow, newCol, newCost});
                    }
                }
            }
        }

        return -1; // Should not reach here if a path exists
    }

    private static int[][] expandGrid(int[][] grid, int factor) {
        int originalRows = grid.length;
        int originalCols = grid[0].length;
        int newRows = originalRows * factor;
        int newCols = originalCols * factor;
        int[][] expandedGrid = new int[newRows][newCols];

        for (int i = 0; i < newRows; i++) {
            for (int j = 0; j < newCols; j++) {
                int originalValue = grid[i % originalRows][j % originalCols];
                int increment = i / originalRows + j / originalCols;
                int newValue = (originalValue + increment - 1) % 9 + 1;
                expandedGrid[i][j] = newValue;
            }
        }

        return expandedGrid;
    }
}
