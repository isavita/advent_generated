
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ParabolicReflectorDish {

    public static void main(String[] args) {
        try {
            List<String> lines = readInput("input.txt");
            char[][] grid = parseGrid(lines);

            // Part 1
            char[][] tiltedGrid = tiltNorth(grid);
            int totalLoad = calculateLoad(tiltedGrid);
            System.out.println("Part 1 - Total load after tilting north: " + totalLoad);

            // Part 2
            long cycles = 1000000000;
            char[][] cycledGrid = runSpinCycles(grid, cycles);
            int totalLoadAfterCycles = calculateLoad(cycledGrid);
            System.out.println("Part 2 - Total load after " + cycles + " cycles: " + totalLoadAfterCycles);

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

    private static char[][] tiltNorth(char[][] grid) {
        int rows = grid.length;
        int cols = grid[0].length;
        char[][] newGrid = new char[rows][cols];
        for (int i = 0; i < rows; i++) {
            System.arraycopy(grid[i], 0, newGrid[i], 0, cols);
        }

        for (int j = 0; j < cols; j++) {
            int nextEmpty = 0;
            for (int i = 0; i < rows; i++) {
                if (newGrid[i][j] == 'O') {
                    if (nextEmpty != i) {
                        newGrid[nextEmpty][j] = 'O';
                        newGrid[i][j] = '.';
                    }
                    nextEmpty++;
                } else if (newGrid[i][j] == '#') {
                    nextEmpty = i + 1;
                }
            }
        }
        return newGrid;
    }
    
    private static char[][] tiltSouth(char[][] grid) {
        int rows = grid.length;
        int cols = grid[0].length;
        char[][] newGrid = new char[rows][cols];
        for (int i = 0; i < rows; i++) {
            System.arraycopy(grid[i], 0, newGrid[i], 0, cols);
        }
        for (int j = 0; j < cols; j++) {
            int nextEmpty = rows - 1;
            for (int i = rows - 1; i >= 0; i--) {
                if (newGrid[i][j] == 'O') {
                    if (nextEmpty != i) {
                        newGrid[nextEmpty][j] = 'O';
                        newGrid[i][j] = '.';
                    }
                    nextEmpty--;
                } else if (newGrid[i][j] == '#') {
                    nextEmpty = i - 1;
                }
            }
        }
        return newGrid;
    }

    private static char[][] tiltWest(char[][] grid) {
        int rows = grid.length;
        int cols = grid[0].length;
        char[][] newGrid = new char[rows][cols];
        for (int i = 0; i < rows; i++) {
            System.arraycopy(grid[i], 0, newGrid[i], 0, cols);
        }
        for (int i = 0; i < rows; i++) {
            int nextEmpty = 0;
            for (int j = 0; j < cols; j++) {
                if (newGrid[i][j] == 'O') {
                    if (nextEmpty != j) {
                        newGrid[i][nextEmpty] = 'O';
                        newGrid[i][j] = '.';
                    }
                    nextEmpty++;
                } else if (newGrid[i][j] == '#') {
                    nextEmpty = j + 1;
                }
            }
        }
        return newGrid;
    }

    private static char[][] tiltEast(char[][] grid) {
        int rows = grid.length;
        int cols = grid[0].length;
        char[][] newGrid = new char[rows][cols];
        for (int i = 0; i < rows; i++) {
            System.arraycopy(grid[i], 0, newGrid[i], 0, cols);
        }
        for (int i = 0; i < rows; i++) {
            int nextEmpty = cols - 1;
            for (int j = cols - 1; j >= 0; j--) {
                if (newGrid[i][j] == 'O') {
                    if (nextEmpty != j) {
                        newGrid[i][nextEmpty] = 'O';
                        newGrid[i][j] = '.';
                    }
                    nextEmpty--;
                } else if (newGrid[i][j] == '#') {
                    nextEmpty = j - 1;
                }
            }
        }
        return newGrid;
    }

    private static char[][] runSpinCycles(char[][] grid, long numCycles) {
        Map<String, Long> seenStates = new HashMap<>();
        List<String> stateOrder = new ArrayList<>();
        char[][] currentGrid = grid;
        long cycleStart = -1;
        long cycleLength = -1;

        for (long i = 0; i < numCycles; i++) {
            String gridString = gridToString(currentGrid);
            if (seenStates.containsKey(gridString)) {
                cycleStart = seenStates.get(gridString);
                cycleLength = i - cycleStart;
                break;
            } else {
                seenStates.put(gridString, i);
                stateOrder.add(gridString);
            }

            currentGrid = tiltNorth(currentGrid);
            currentGrid = tiltWest(currentGrid);
            currentGrid = tiltSouth(currentGrid);
            currentGrid = tiltEast(currentGrid);
        }

        if (cycleLength > 0) {
            long remainingCycles = (numCycles - cycleStart) % cycleLength;
            String finalState = stateOrder.get((int) (cycleStart + remainingCycles));
            return parseGrid(List.of(finalState.split("\n")));
        }

        return currentGrid;
    }

    private static String gridToString(char[][] grid) {
        StringBuilder sb = new StringBuilder();
        for (char[] row : grid) {
            sb.append(new String(row)).append("\n");
        }
        return sb.toString();
    }

    private static int calculateLoad(char[][] grid) {
        int totalLoad = 0;
        int rows = grid.length;
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < grid[i].length; j++) {
                if (grid[i][j] == 'O') {
                    totalLoad += rows - i;
                }
            }
        }
        return totalLoad;
    }
}
