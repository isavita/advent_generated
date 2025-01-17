
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class LavaProductionFacility {

    public static void main(String[] args) {
        try {
            List<String> grid = readInput("input.txt");
            int part1Result = calculateEnergizedTiles(grid, 0, 0, 0);
            System.out.println(part1Result);

            int part2Result = findMaxEnergizedTiles(grid);
            System.out.println(part2Result);
        } catch (IOException e) {
            e.printStackTrace();
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

    private static int calculateEnergizedTiles(List<String> grid, int startRow, int startCol, int startDir) {
        Set<String> energized = new HashSet<>();
        Set<String> visited = new HashSet<>();
        traceBeam(grid, startRow, startCol, startDir, energized, visited);
        return energized.size();
    }

    private static void traceBeam(List<String> grid, int row, int col, int dir, Set<String> energized, Set<String> visited) {
        int rows = grid.size();
        int cols = grid.get(0).length();

        while (row >= 0 && row < rows && col >= 0 && col < cols) {
            String cell = grid.get(row).substring(col, col + 1);
            String state = row + "," + col + "," + dir;

            if (visited.contains(state)) {
                return;
            }
            visited.add(state);
            energized.add(row + "," + col);

            if (cell.equals(".")) {
                // Continue in the same direction
            } else if (cell.equals("/")) {
                dir = reflect(dir, "/");
            } else if (cell.equals("\\")) {
                dir = reflect(dir, "\\");
            } else if (cell.equals("|")) {
                if (dir == 1 || dir == 3) {
                    traceBeam(grid, row - 1, col, 0, energized, visited);
                    traceBeam(grid, row + 1, col, 2, energized, visited);
                    return;
                }
            } else if (cell.equals("-")) {
                if (dir == 0 || dir == 2) {
                    traceBeam(grid, row, col - 1, 3, energized, visited);
                    traceBeam(grid, row, col + 1, 1, energized, visited);
                    return;
                }
            }

            if (dir == 0) {
                row--;
            } else if (dir == 1) {
                col++;
            } else if (dir == 2) {
                row++;
            } else if (dir == 3) {
                col--;
            }
        }
    }

    private static int reflect(int dir, String mirror) {
        if (mirror.equals("/")) {
            if (dir == 0) return 1;
            if (dir == 1) return 0;
            if (dir == 2) return 3;
            if (dir == 3) return 2;
        } else if (mirror.equals("\\")) {
            if (dir == 0) return 3;
            if (dir == 1) return 2;
            if (dir == 2) return 1;
            if (dir == 3) return 0;
        }
        return -1;
    }

    private static int findMaxEnergizedTiles(List<String> grid) {
        int maxEnergized = 0;
        int rows = grid.size();
        int cols = grid.get(0).length();

        // Top row
        for (int col = 0; col < cols; col++) {
            maxEnergized = Math.max(maxEnergized, calculateEnergizedTiles(grid, 0, col, 2));
        }

        // Bottom row
        for (int col = 0; col < cols; col++) {
            maxEnergized = Math.max(maxEnergized, calculateEnergizedTiles(grid, rows - 1, col, 0));
        }

        // Left column
        for (int row = 0; row < rows; row++) {
            maxEnergized = Math.max(maxEnergized, calculateEnergizedTiles(grid, row, 0, 1));
        }

        // Right column
        for (int row = 0; row < rows; row++) {
            maxEnergized = Math.max(maxEnergized, calculateEnergizedTiles(grid, row, cols - 1, 3));
        }

        return maxEnergized;
    }
}
