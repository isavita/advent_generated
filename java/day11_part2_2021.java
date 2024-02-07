
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class solution {
    public static void main(String[] args) {
        int[][] grid = readInput("input.txt");

        int step = 0;
        while (true) {
            step++;
            int flashes = simulateStep(grid);
            if (flashes == 100) {
                break;
            }
        }

        System.out.println(step);
    }

    public static int[][] readInput(String filename) {
        try {
            File file = new File(filename);
            Scanner scanner = new Scanner(file);

            int rows = 0;
            while (scanner.hasNextLine()) {
                scanner.nextLine();
                rows++;
            }

            int[][] grid = new int[rows][];
            scanner = new Scanner(file);
            int rowIdx = 0;
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                int[] row = new int[line.length()];
                for (int i = 0; i < line.length(); i++) {
                    row[i] = Character.getNumericValue(line.charAt(i));
                }
                grid[rowIdx] = row;
                rowIdx++;
            }

            return grid;
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            return null;
        }
    }

    public static int simulateStep(int[][] grid) {
        int flashes = 0;
        boolean[][] flashed = new boolean[grid.length][grid[0].length];

        for (int y = 0; y < grid.length; y++) {
            for (int x = 0; x < grid[y].length; x++) {
                grid[y][x]++;
            }
        }

        for (int y = 0; y < grid.length; y++) {
            for (int x = 0; x < grid[y].length; x++) {
                if (grid[y][x] > 9) {
                    flashes += flash(grid, x, y, flashed);
                }
            }
        }

        for (int y = 0; y < grid.length; y++) {
            for (int x = 0; x < grid[y].length; x++) {
                if (flashed[y][x]) {
                    grid[y][x] = 0;
                }
            }
        }

        return flashes;
    }

    public static int flash(int[][] grid, int x, int y, boolean[][] flashed) {
        if (flashed[y][x]) {
            return 0;
        }

        flashed[y][x] = true;
        int flashes = 1;
        int[][] directions = {{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}};

        for (int[] dir : directions) {
            int newX = x + dir[0];
            int newY = y + dir[1];
            if (newX >= 0 && newX < grid[0].length && newY >= 0 && newY < grid.length) {
                grid[newY][newX]++;
                if (grid[newY][newX] > 9) {
                    flashes += flash(grid, newX, newY, flashed);
                }
            }
        }

        return flashes;
    }
}
