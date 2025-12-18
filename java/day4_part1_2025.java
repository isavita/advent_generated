
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class PrintingDepartment {

    public static void main(String[] args) {
        String fileName = "input.txt";
        List<String> grid = new ArrayList<>();

        // Read the input grid from the file
        try (BufferedReader br = new BufferedReader(new FileReader(fileName))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (!line.trim().isEmpty()) {
                    grid.add(line.trim());
                }
            }
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
            return;
        }

        if (grid.isEmpty()) {
            System.out.println(0);
            return;
        }

        int accessibleCount = countAccessibleRolls(grid);
        System.out.println(accessibleCount);
    }

    /**
     * Iterates through the grid and counts '@' characters that meet the 
     * accessibility criteria (fewer than 4 adjacent '@' symbols).
     */
    private static int countAccessibleRolls(List<String> grid) {
        int rows = grid.size();
        int cols = grid.get(0).length();
        int totalAccessible = 0;

        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                if (grid.get(r).charAt(c) == '@') {
                    if (countNeighbors(grid, r, c, rows, cols) < 4) {
                        totalAccessible++;
                    }
                }
            }
        }
        return totalAccessible;
    }

    /**
     * Checks the 8 neighboring cells for the '@' character.
     */
    private static int countNeighbors(List<String> grid, int row, int col, int maxR, int maxC) {
        int count = 0;

        // Check all 8 directions (offsets from -1 to 1)
        for (int dr = -1; dr <= 1; dr++) {
            for (int dc = -1; dc <= 1; dc++) {
                // Skip the cell itself
                if (dr == 0 && dc == 0) continue;

                int nr = row + dr;
                int nc = col + dc;

                // Boundary check
                if (nr >= 0 && nr < maxR && nc >= 0 && nc < maxC) {
                    if (grid.get(nr).charAt(nc) == '@') {
                        count++;
                    }
                }
            }
        }
        return count;
    }
}
