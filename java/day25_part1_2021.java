
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class SeaCucumber {

    public static void main(String[] args) {
        char[][] grid = readInput("input.txt");
        int steps = 0;
        boolean moved;

        do {
            moved = false;
            char[][] nextGrid = new char[grid.length][grid[0].length];
            for (int i = 0; i < grid.length; i++) {
                System.arraycopy(grid[i], 0, nextGrid[i], 0, grid[i].length);
            }

            // Move east-facing cucumbers
            for (int i = 0; i < grid.length; i++) {
                for (int j = 0; j < grid[i].length; j++) {
                    if (grid[i][j] == '>' && grid[i][(j + 1) % grid[i].length] == '.') {
                        nextGrid[i][j] = '.';
                        nextGrid[i][(j + 1) % grid[i].length] = '>';
                        moved = true;
                    }
                }
            }
            
            grid = nextGrid;
            nextGrid = new char[grid.length][grid[0].length];
            for (int i = 0; i < grid.length; i++) {
                System.arraycopy(grid[i], 0, nextGrid[i], 0, grid[i].length);
            }

            // Move south-facing cucumbers
            for (int i = 0; i < grid.length; i++) {
                for (int j = 0; j < grid[i].length; j++) {
                    if (grid[i][j] == 'v' && grid[(i + 1) % grid.length][j] == '.') {
                        nextGrid[i][j] = '.';
                        nextGrid[(i + 1) % grid.length][j] = 'v';
                        moved = true;
                    }
                }
            }

            grid = nextGrid;
            steps++;
        } while (moved);

        System.out.println(steps);
    }

    private static char[][] readInput(String filename) {
        try {
            File file = new File(filename);
            Scanner scanner = new Scanner(file);
            int rows = 0;
            int cols = 0;
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                cols = line.length();
                rows++;
            }
            scanner.close();

            char[][] grid = new char[rows][cols];
            scanner = new Scanner(file);
            int row = 0;
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                grid[row] = line.toCharArray();
                row++;
            }
            scanner.close();
            return grid;
        } catch (FileNotFoundException e) {
            System.out.println("File not found: " + filename);
            return null;
        }
    }
}
