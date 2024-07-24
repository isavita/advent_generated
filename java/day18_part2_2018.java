
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Main {
    private static final char OPEN = '.';
    private static final char TREES = '|';
    private static final char LUMBERYARD = '#';
    private static final int SIZE = 50;

    public static void main(String[] args) throws IOException {
        char[][] grid = readInput("input.txt");
        Map<String, Integer> seenStates = new HashMap<>();
        int cycleStart = 0, cycleLength = 0;

        for (int minute = 0; ; minute++) {
            String state = gridToString(grid);
            if (seenStates.containsKey(state)) {
                cycleStart = seenStates.get(state);
                cycleLength = minute - cycleStart;
                break;
            }
            seenStates.put(state, minute);
            grid = transform(grid);
        }

        int remainingMinutes = (1_000_000_000 - cycleStart) % cycleLength;
        for (int i = 0; i < remainingMinutes; i++) {
            grid = transform(grid);
        }

        int[] resources = countResources(grid);
        System.out.println(resources[0] * resources[1]);
    }

    private static char[][] readInput(String filename) throws IOException {
        List<String> lines = Files.readAllLines(Paths.get(filename));
        char[][] grid = new char[lines.size()][];
        for (int i = 0; i < lines.size(); i++) {
            grid[i] = lines.get(i).toCharArray();
        }
        return grid;
    }

    private static char[][] transform(char[][] grid) {
        int rows = grid.length, cols = grid[0].length;
        char[][] newGrid = new char[rows][cols];
        for (int i = 0; i < rows; i++) {
            System.arraycopy(grid[i], 0, newGrid[i], 0, cols);
            for (int j = 0; j < cols; j++) {
                newGrid[i][j] = nextAcreState(grid, i, j);
            }
        }
        return newGrid;
    }

    private static char nextAcreState(char[][] grid, int i, int j) {
        switch (grid[i][j]) {
            case OPEN:
                return countAdjacent(grid, i, j, TREES) >= 3 ? TREES : OPEN;
            case TREES:
                return countAdjacent(grid, i, j, LUMBERYARD) >= 3 ? LUMBERYARD : TREES;
            case LUMBERYARD:
                return (countAdjacent(grid, i, j, LUMBERYARD) >= 1 && countAdjacent(grid, i, j, TREES) >= 1) ? LUMBERYARD : OPEN;
        }
        return grid[i][j];
    }

    private static int countAdjacent(char[][] grid, int i, int j, char acreType) {
        int count = 0;
        for (int x = -1; x <= 1; x++) {
            for (int y = -1; y <= 1; y++) {
                if (x == 0 && y == 0) continue;
                int newX = i + x, newY = j + y;
                if (newX >= 0 && newX < grid.length && newY >= 0 && newY < grid[0].length && grid[newX][newY] == acreType) {
                    count++;
                }
            }
        }
        return count;
    }

    private static int[] countResources(char[][] grid) {
        int wooded = 0, lumberyards = 0;
        for (char[] row : grid) {
            for (char acre : row) {
                if (acre == TREES) wooded++;
                else if (acre == LUMBERYARD) lumberyards++;
            }
        }
        return new int[]{wooded, lumberyards};
    }

    private static String gridToString(char[][] grid) {
        StringBuilder sb = new StringBuilder();
        for (char[] row : grid) {
            sb.append(row);
            sb.append('\n');
        }
        return sb.toString();
    }
}
