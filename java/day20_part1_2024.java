
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Queue;

public class RaceCondition {

    private static final int[] DX = {0, 0, 1, -1};
    private static final int[] DY = {1, -1, 0, 0};

    public static void main(String[] args) {
        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
            List<String> lines = new ArrayList<>();
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }

            char[][] grid = new char[lines.size()][lines.get(0).length()];
            int startX = -1, startY = -1, endX = -1, endY = -1;
            for (int i = 0; i < lines.size(); i++) {
                grid[i] = lines.get(i).toCharArray();
                for (int j = 0; j < grid[i].length; j++) {
                    if (grid[i][j] == 'S') {
                        startX = i;
                        startY = j;
                    } else if (grid[i][j] == 'E') {
                        endX = i;
                        endY = j;
                    }
                }
            }

            int shortestPath = bfs(grid, startX, startY, endX, endY, false);
            Map<Integer, Integer> cheatSavings = findCheats(grid, startX, startY, endX, endY, shortestPath);

            int count = 0;
            for (int savings : cheatSavings.keySet()) {
                if (savings >= 100) {
                    count += cheatSavings.get(savings);
                }
            }

            System.out.println(count);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static Map<Integer, Integer> findCheats(char[][] grid, int startX, int startY, int endX, int endY, int shortestPath) {
        Map<Integer, Integer> cheatSavings = new HashMap<>();
        for (int i = 0; i < grid.length; i++) {
            for (int j = 0; j < grid[i].length; j++) {
                if (grid[i][j] == '.' || grid[i][j] == 'S') {
                    for (int cheatLen = 1; cheatLen <= 2; cheatLen++) {
                        for (int dir1 = 0; dir1 < 4; dir1++) {
                            for (int dir2 = 0; dir2 < 4; dir2++) {
                                int cheatStartX = i;
                                int cheatStartY = j;
                                int cheatEndX = i;
                                int cheatEndY = j;
                                
                                if (cheatLen >= 1) {
                                    cheatEndX += DX[dir1];
                                    cheatEndY += DY[dir1];
                                }
                                if (cheatLen >= 2) {
                                    cheatEndX += DX[dir2];
                                    cheatEndY += DY[dir2];
                                }

                                if (isValid(grid, cheatEndX, cheatEndY) && (grid[cheatEndX][cheatEndY] == '.' || grid[cheatEndX][cheatEndY] == 'E')) {
                                    int pathWithCheat = bfs(grid, startX, startY, cheatStartX, cheatStartY, false) + cheatLen + bfs(grid, cheatEndX, cheatEndY, endX, endY, false);
                                    if (pathWithCheat < shortestPath) {
                                        int savings = shortestPath - pathWithCheat;
                                        cheatSavings.put(savings, cheatSavings.getOrDefault(savings, 0) + 1);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        return cheatSavings;
    }

    private static int bfs(char[][] grid, int startX, int startY, int endX, int endY, boolean canCheat) {
        int rows = grid.length;
        int cols = grid[0].length;
        int[][][] dist = new int[rows][cols][2];
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                Arrays.fill(dist[i][j], -1);
            }
        }

        Queue<int[]> queue = new ArrayDeque<>();
        queue.offer(new int[]{startX, startY, canCheat ? 1 : 0});
        dist[startX][startY][canCheat ? 1 : 0] = 0;

        while (!queue.isEmpty()) {
            int[] curr = queue.poll();
            int x = curr[0];
            int y = curr[1];
            int cheat = curr[2];

            if (x == endX && y == endY) {
                return dist[x][y][cheat];
            }

            for (int i = 0; i < 4; i++) {
                int nx = x + DX[i];
                int ny = y + DY[i];

                if (isValid(grid, nx, ny)) {
                    if (cheat == 1 && (grid[nx][ny] == '.' || grid[nx][ny] == '#' || grid[nx][ny] == 'E' || grid[nx][ny] == 'S') && dist[nx][ny][cheat] == -1) {
                        dist[nx][ny][cheat] = dist[x][y][cheat] + 1;
                        queue.offer(new int[]{nx, ny, cheat});
                    } else if (cheat == 0 && (grid[nx][ny] == '.' || grid[nx][ny] == 'E' || grid[nx][ny] == 'S') && dist[nx][ny][cheat] == -1) {
                        dist[nx][ny][cheat] = dist[x][y][cheat] + 1;
                        queue.offer(new int[]{nx, ny, cheat});
                    }
                }
            }
        }

        return Integer.MAX_VALUE;
    }

    private static boolean isValid(char[][] grid, int x, int y) {
        return x >= 0 && x < grid.length && y >= 0 && y < grid[0].length;
    }
}
