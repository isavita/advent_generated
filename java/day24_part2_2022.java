
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Queue;
import java.util.Set;

public class BlizzardBasin {

    private static class State {
        int row;
        int col;
        int time;

        public State(int row, int col, int time) {
            this.row = row;
            this.col = col;
            this.time = time;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            State state = (State) o;
            return row == state.row && col == state.col && time == state.time;
        }

        @Override
        public int hashCode() {
            return Objects.hash(row, col, time);
        }
    }

    public static void main(String[] args) {
        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
            List<String> lines = new ArrayList<>();
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }

            char[][] grid = new char[lines.size()][lines.get(0).length()];
            for (int i = 0; i < lines.size(); i++) {
                grid[i] = lines.get(i).toCharArray();
            }

            int startRow = 0;
            int startCol = 1;
            int endRow = grid.length - 1;
            int endCol = grid[0].length - 2;

            int time1 = solve(grid, startRow, startCol, endRow, endCol, 0);
            int time2 = solve(grid, endRow, endCol, startRow, startCol, time1);
            int time3 = solve(grid, startRow, startCol, endRow, endCol, time2);

            System.out.println("Part 1: " + solve(grid, startRow, startCol, endRow, endCol, 0));
            System.out.println("Part 2: " + time3);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static int solve(char[][] grid, int startRow, int startCol, int endRow, int endCol, int startTime) {
        int rows = grid.length;
        int cols = grid[0].length;

        Queue<State> queue = new ArrayDeque<>();
        queue.add(new State(startRow, startCol, startTime));

        Set<State> visited = new HashSet<>();
        visited.add(new State(startRow, startCol, startTime));

        int[][] directions = {{0, 0}, {0, 1}, {0, -1}, {1, 0}, {-1, 0}}; // Stay, Right, Left, Down, Up

        while (!queue.isEmpty()) {
            State current = queue.poll();
            int row = current.row;
            int col = current.col;
            int time = current.time;

            if (row == endRow && col == endCol) {
                return time;
            }

            for (int[] dir : directions) {
                int newRow = row + dir[0];
                int newCol = col + dir[1];
                int newTime = time + 1;

                if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols && grid[newRow][newCol] != '#') {
                    // Check for blizzards at the new position at the new time
                    boolean blizzardPresent = false;
                    for (int i = 1; i < rows - 1; i++) {
                        for (int j = 1; j < cols - 1; j++) {
                            if (grid[i][j] != '.') {
                                int blizzardRow = i;
                                int blizzardCol = j;

                                switch (grid[i][j]) {
                                    case '^':
                                        blizzardRow = 1 + ((i - 1 - newTime) % (rows - 2) + (rows - 2)) % (rows - 2);
                                        if (blizzardRow == newRow && blizzardCol == newCol) blizzardPresent = true;
                                        break;
                                    case 'v':
                                        blizzardRow = 1 + ((i - 1 + newTime) % (rows - 2) + (rows - 2)) % (rows - 2);
                                        if (blizzardRow == newRow && blizzardCol == newCol) blizzardPresent = true;
                                        break;
                                    case '<':
                                        blizzardCol = 1 + ((j - 1 - newTime) % (cols - 2) + (cols - 2)) % (cols - 2);
                                        if (blizzardRow == newRow && blizzardCol == newCol) blizzardPresent = true;
                                        break;
                                    case '>':
                                        blizzardCol = 1 + ((j - 1 + newTime) % (cols - 2) + (cols - 2)) % (cols - 2);
                                        if (blizzardRow == newRow && blizzardCol == newCol) blizzardPresent = true;
                                        break;

                                }
                                if(blizzardPresent) break;
                            }
                        }
                        if(blizzardPresent) break;
                    }

                    if (!blizzardPresent) {
                        State newState = new State(newRow, newCol, newTime);
                        if (!visited.contains(newState)) {
                            queue.add(newState);
                            visited.add(newState);
                        }
                    }
                }
            }
        }

        return -1; // Should never happen if a solution exists
    }
}
