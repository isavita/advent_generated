
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class LongestHike {

    private static int maxSteps = 0;

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            List<String> lines = new ArrayList<>();
            String line;
            while ((line = br.readLine()) != null) {
                lines.add(line);
            }

            char[][] grid = new char[lines.size()][lines.get(0).length()];
            for (int i = 0; i < lines.size(); i++) {
                grid[i] = lines.get(i).toCharArray();
            }

            int startX = 0;
            for (int i = 0; i < grid[0].length; i++) {
                if (grid[0][i] == '.') {
                    startX = i;
                    break;
                }
            }

            dfs(grid, 0, startX, 0, new boolean[grid.length][grid[0].length]);

            System.out.println(maxSteps);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static void dfs(char[][] grid, int row, int col, int steps, boolean[][] visited) {
        if (row < 0 || row >= grid.length || col < 0 || col >= grid[0].length ||
                grid[row][col] == '#' || visited[row][col]) {
            return;
        }

        if (row == grid.length - 1) {
            maxSteps = Math.max(maxSteps, steps);
            return;
        }

        visited[row][col] = true;

        if (grid[row][col] == '.') {
            dfs(grid, row + 1, col, steps + 1, visited);
            dfs(grid, row - 1, col, steps + 1, visited);
            dfs(grid, row, col + 1, steps + 1, visited);
            dfs(grid, row, col - 1, steps + 1, visited);
        } else if (grid[row][col] == 'v') {
            dfs(grid, row + 1, col, steps + 1, visited);
        } else if (grid[row][col] == '^') {
            dfs(grid, row - 1, col, steps + 1, visited);
        } else if (grid[row][col] == '>') {
            dfs(grid, row, col + 1, steps + 1, visited);
        } else if (grid[row][col] == '<') {
            dfs(grid, row, col - 1, steps + 1, visited);
        }

        visited[row][col] = false;
    }
}
