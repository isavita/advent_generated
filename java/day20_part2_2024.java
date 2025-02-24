
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Queue;

public class Solution {

  private static final int[] dx = {1, -1, 0, 0};
  private static final int[] dy = {0, 0, 1, -1};

  public static void main(String[] args) {
    solve();
  }

  private static void solve() {
    try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
      List<String> grid = new ArrayList<>();
      String line;
      while ((line = reader.readLine()) != null) {
        grid.add(line.strip());
      }

      int h = grid.size();
      int w = grid.get(0).length();
      boolean[][] walls = new boolean[h][w];
      List<int[]> trackCells = new ArrayList<>();
      int[] S = null;
      int[] E = null;

      for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
          char c = grid.get(i).charAt(j);
          if (c == 'S') {
            S = new int[] {i, j};
          } else if (c == 'E') {
            E = new int[] {i, j};
          }
          if (c == '#') {
            walls[i][j] = true;
          } else {
            trackCells.add(new int[] {i, j});
          }
        }
      }

      int[][] distFromS = bfs(S, h, w, walls);
      int[][] distFromE = bfs(E, h, w, walls);

      if (distFromS[E[0]][E[1]] == -1) {
        System.out.println(0);
        return;
      }

      int normalCost = distFromS[E[0]][E[1]];
      Map<String, Integer> cheats = new HashMap<>();

      for (int[] startCell : trackCells) {
        int startX = startCell[0];
        int startY = startCell[1];
        int sd = distFromS[startX][startY];
        if (sd == -1) {
          continue;
        }

        int[][] distC = new int[h][w];
        for (int i = 0; i < h; i++) {
          for (int j = 0; j < w; j++) {
            distC[i][j] = -1;
          }
        }

        distC[startX][startY] = 0;
        Queue<int[]> q = new ArrayDeque<>();
        q.offer(new int[] {startX, startY});

        while (!q.isEmpty()) {
          int[] curr = q.poll();
          int x = curr[0];
          int y = curr[1];
          int steps = distC[x][y];
          if (steps == 20) {
            continue;
          }

          for (int i = 0; i < 4; i++) {
            int nx = x + dx[i];
            int ny = y + dy[i];
            if (nx >= 0 && nx < h && ny >= 0 && ny < w && distC[nx][ny] == -1) {
              distC[nx][ny] = steps + 1;
              q.offer(new int[] {nx, ny});
            }
          }
        }

        for (int x = 0; x < h; x++) {
          for (int y = 0; y < w; y++) {
            int s = distC[x][y];
            if (s > 0 && s <= 20 && !walls[x][y]) {
              int ed = distFromE[x][y];
              if (ed == -1) {
                continue;
              }
              int cost = sd + s + ed;
              if (cost < normalCost) {
                String key = startX + "," + startY + "," + x + "," + y;
                if (!cheats.containsKey(key) || cost < cheats.get(key)) {
                  cheats.put(key, cost);
                }
              }
            }
          }
        }
      }

      int count = 0;
      for (int cost : cheats.values()) {
        if (normalCost - cost >= 100) {
          count++;
        }
      }
      System.out.println(count);

    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  private static int[][] bfs(int[] start, int h, int w, boolean[][] walls) {
    int[][] dist = new int[h][w];
    for (int i = 0; i < h; i++) {
      for (int j = 0; j < w; j++) {
        dist[i][j] = -1;
      }
    }

    dist[start[0]][start[1]] = 0;
    Queue<int[]> q = new ArrayDeque<>();
    q.offer(start);

    while (!q.isEmpty()) {
      int[] curr = q.poll();
      int x = curr[0];
      int y = curr[1];

      for (int i = 0; i < 4; i++) {
        int nx = x + dx[i];
        int ny = y + dy[i];
        if (nx >= 0 && nx < h && ny >= 0 && ny < w && !walls[nx][ny] && dist[nx][ny] == -1) {
          dist[nx][ny] = dist[x][y] + 1;
          q.offer(new int[] {nx, ny});
        }
      }
    }
    return dist;
  }
}
