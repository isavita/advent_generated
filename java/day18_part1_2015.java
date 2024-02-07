
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class solution {
    private static final int gridSize = 100;
    private static final int steps = 100;

    private static int countOnNeighbors(boolean[][] grid, int x, int y) {
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

    private static boolean[][] step(boolean[][] grid) {
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

        return newGrid;
    }

    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);

            boolean[][] grid = new boolean[gridSize][gridSize];
            int y = 0;
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                for (int x = 0; x < line.length(); x++) {
                    grid[x][y] = line.charAt(x) == '#';
                }
                y++;
            }

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

            scanner.close();
        } catch (FileNotFoundException e) {
            System.out.println("Error opening file: " + e);
        }
    }
}
