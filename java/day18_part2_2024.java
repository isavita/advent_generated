
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedList;
import java.util.Queue;

public class RAMRun {

    private static final int GRID_SIZE = 71;
    private static final int[] dx = {0, 0, 1, -1};
    private static final int[] dy = {1, -1, 0, 0};

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            int[][] grid = new int[GRID_SIZE][GRID_SIZE];
            int byteCount = 0;
            int minSteps = -1;
            int cutoffX = -1, cutoffY = -1;

            while ((line = br.readLine()) != null) {
                String[] parts = line.split(",");
                int x = Integer.parseInt(parts[0]);
                int y = Integer.parseInt(parts[1]);
                byteCount++;
                grid[y][x] = 1;

                if (byteCount <= 1024) {
                    minSteps = bfs(grid);
                }

                if (cutoffX == -1 && bfs(grid) == -1) {
                    cutoffX = x;
                    cutoffY = y;
                }
            }

            System.out.println(minSteps);
            System.out.println(cutoffX + "," + cutoffY);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static int bfs(int[][] grid) {
        boolean[][] visited = new boolean[GRID_SIZE][GRID_SIZE];
        Queue<int[]> queue = new LinkedList<>();
        queue.offer(new int[]{0, 0, 0});
        visited[0][0] = true;

        while (!queue.isEmpty()) {
            int[] curr = queue.poll();
            int x = curr[0];
            int y = curr[1];
            int steps = curr[2];

            if (x == GRID_SIZE - 1 && y == GRID_SIZE - 1) {
                return steps;
            }

            for (int i = 0; i < 4; i++) {
                int nx = x + dx[i];
                int ny = y + dy[i];

                if (nx >= 0 && nx < GRID_SIZE && ny >= 0 && ny < GRID_SIZE && grid[ny][nx] == 0 && !visited[ny][nx]) {
                    visited[ny][nx] = true;
                    queue.offer(new int[]{nx, ny, steps + 1});
                }
            }
        }

        return -1;
    }
}
