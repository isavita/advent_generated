
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    static final int gridSize = 100;
    static final int steps = 100;

    public static int countOnNeighbors(boolean[][] grid, int x, int y) {
        int on = 0;
        for (int dx = -1; dx <= 1; dx++) {
            for (int dy = -1; dy <= 1; dy++) {
                if (dx == 0 && dy == 0) {
                    continue;
                }
                int nx = x + dx;
                int ny = y + dy;
                if (nx >= 0 && nx < gridSize && ny >= 0 && ny < gridSize && grid[nx][ny]) {
                    on++;
                }
            }
        }
        return on;
    }

    public static boolean[][] step(boolean[][] grid) {
        boolean[][] newGrid = new boolean[gridSize][gridSize];

        for (int x = 0; x < gridSize; x++) {
            for (int y = 0; y < gridSize; y++) {
                int onNeighbors = countOnNeighbors(grid, x, y);
                if (grid[x][y]) {
                    newGrid[x][y] = onNeighbors == 2 || onNeighbors == 3;
                } else {
                    newGrid[x][y] = onNeighbors == 3;
                }
            }
        }

        // Ensure corners are always on
        newGrid[0][0] = true;
        newGrid[0][gridSize - 1] = true;
        newGrid[gridSize - 1][0] = true;
        newGrid[gridSize - 1][gridSize - 1] = true;

        return newGrid;
    }

    public static void main(String[] args) {
        boolean[][] grid = new boolean[gridSize][gridSize];

        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            int y = 0;
            while ((line = reader.readLine()) != null) {
                for (int x = 0; x < line.length(); x++) {
                    grid[x][y] = line.charAt(x) == '#';
                }
                y++;
            }
            reader.close();
        } catch (IOException e) {
            System.out.println("Error reading file: " + e);
            return;
        }

        // Initialize corners as always on
        grid[0][0] = true;
        grid[0][gridSize - 1] = true;
        grid[gridSize - 1][0] = true;
        grid[gridSize - 1][gridSize - 1] = true;

        for (int i = 0; i < steps; i++) {
            grid = step(grid);
        }

        int onCount = 0;
        for (boolean[] row : grid) {
            for (boolean light : row) {
                if (light) {
                    onCount++;
                }
            }
        }

        System.out.println(onCount);
    }
}
