
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Solution {

  static final int[] DR = {1, -1, 0, 0};
  static final int[] DC = {0, 0, 1, -1};

  public static void main(String[] args) throws IOException {
    List<String> lines = Files.readAllLines(Paths.get("input.txt"));
    int nr = lines.size();
    int nc = lines.get(0).length();
    int[][] grid = new int[nr][nc];
    for (int r = 0; r < nr; r++) {
      for (int c = 0; c < nc; c++) {
        grid[r][c] = lines.get(r).charAt(c) - '0';
      }
    }

    int sumScores = 0;
    for (int r = 0; r < nr; r++) {
      for (int c = 0; c < nc; c++) {
        if (grid[r][c] == 0) {
          sumScores += bfs(grid, r, c, nr, nc);
        }
      }
    }
    System.out.println(sumScores);
  }

  static int bfs(int[][] grid, int sr, int sc, int nr, int nc) {
    Set<Long> reached = new HashSet<>();
    Deque<long[]> front = new ArrayDeque<>();
    Set<Long> visited = new HashSet<>();

    front.add(new long[] {sr, sc, 0});

    while (!front.isEmpty()) {
      long[] cur = front.poll();
      int r = (int) cur[0];
      int c = (int) cur[1];
      int h = (int) cur[2];

      if (h == 9) {
        reached.add(((long) r << 32) | c);
        continue;
      }

      for (int i = 0; i < 4; i++) {
        int nr2 = r + DR[i];
        int nc2 = c + DC[i];

        if (nr2 >= 0 && nr2 < nr && nc2 >= 0 && nc2 < nc) {
          if (grid[nr2][nc2] == h + 1) {
            long key = (((long) nr2 << 32) | nc2) * 10 + (h + 1);
            if (visited.add(key)) {
              front.add(new long[] {nr2, nc2, h + 1});
            }
          }
        }
      }
    }
    return reached.size();
  }
}
