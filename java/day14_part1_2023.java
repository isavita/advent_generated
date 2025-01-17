
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class ParabolicReflectorDish {

    public static void main(String[] args) {
        try {
            List<String> lines = readInput("input.txt");
            char[][] grid = parseGrid(lines);
            tiltNorth(grid);
            int totalLoad = calculateNorthLoad(grid);
            System.out.println(totalLoad);
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static List<String> readInput(String filename) throws IOException {
        List<String> lines = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }
        }
        return lines;
    }

    private static char[][] parseGrid(List<String> lines) {
        int rows = lines.size();
        int cols = lines.get(0).length();
        char[][] grid = new char[rows][cols];
        for (int i = 0; i < rows; i++) {
            grid[i] = lines.get(i).toCharArray();
        }
        return grid;
    }

    private static void tiltNorth(char[][] grid) {
        int rows = grid.length;
        int cols = grid[0].length;
        for (int col = 0; col < cols; col++) {
            int nextEmptyRow = 0;
            for (int row = 0; row < rows; row++) {
                if (grid[row][col] == 'O') {
                    if (row != nextEmptyRow) {
                        grid[nextEmptyRow][col] = 'O';
                        grid[row][col] = '.';
                    }
                    nextEmptyRow++;
                } else if (grid[row][col] == '#') {
                    nextEmptyRow = row + 1;
                }
            }
        }
    }

    private static int calculateNorthLoad(char[][] grid) {
        int rows = grid.length;
        int cols = grid[0].length;
        int totalLoad = 0;
        for (int row = 0; row < rows; row++) {
            for (int col = 0; col < cols; col++) {
                if (grid[row][col] == 'O') {
                    totalLoad += rows - row;
                }
            }
        }
        return totalLoad;
    }
}
