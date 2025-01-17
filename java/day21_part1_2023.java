
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Set;

public class StepCounter {

    public static void main(String[] args) {
        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
            char[][] grid = reader.lines().map(String::toCharArray).toArray(char[][]::new);
            int[] start = findStart(grid);
            long reachablePlots = countReachablePlots(grid, start, 64);
            System.out.println(reachablePlots);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static int[] findStart(char[][] grid) {
        for (int i = 0; i < grid.length; i++) {
            for (int j = 0; j < grid[i].length; j++) {
                if (grid[i][j] == 'S') {
                    return new int[]{i, j};
                }
            }
        }
        return null;
    }

    private static long countReachablePlots(char[][] grid, int[] start, int steps) {
        int rows = grid.length;
        int cols = grid[0].length;
        int[][] directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

        Queue<int[]> queue = new LinkedList<>();
        queue.offer(new int[]{start[0], start[1], 0});
        Set<String> visited = new HashSet<>();
        visited.add(start[0] + "," + start[1]);
        Set<String> reachable = new HashSet<>();

        while (!queue.isEmpty()) {
            int[] current = queue.poll();
            int row = current[0];
            int col = current[1];
            int step = current[2];

            if (step == steps) {
                reachable.add(row + "," + col);
                continue;
            }

            for (int[] dir : directions) {
                int newRow = row + dir[0];
                int newCol = col + dir[1];

                if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols &&
                        grid[newRow][newCol] != '#' && !visited.contains(newRow + "," + newCol)) {
                    queue.offer(new int[]{newRow, newCol, step + 1});
                    visited.add(newRow + "," + newCol);
                }
            }
        }
        
        Set<String> finalReachable = new HashSet<>();
        for(String s : visited){
            String[] parts = s.split(",");
            int r = Integer.parseInt(parts[0]);
            int c = Integer.parseInt(parts[1]);
            if( (Math.abs(start[0] - r) + Math.abs(start[1] - c)) % 2 == steps % 2){
                finalReachable.add(s);
            }
        }

        return finalReachable.size();
    }
}
