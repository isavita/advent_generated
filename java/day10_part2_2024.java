
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

public class Main {

    static int[][] grid;
    static long[][] dp;
    static int nr, nc;
    static int[][] dirs = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};

    public static void main(String[] args) throws IOException {
        List<String> lines = Files.readAllLines(Paths.get("input.txt"));
        nr = lines.size();
        nc = lines.get(0).length();
        grid = new int[nr][nc];
        dp = new long[nr][nc];

        for (int i = 0; i < nr; i++) {
            String line = lines.get(i);
            for (int j = 0; j < nc; j++) {
                grid[i][j] = line.charAt(j) - '0';
                dp[i][j] = -1;
            }
        }

        long total = 0;
        for (int r = 0; r < nr; r++) {
            for (int c = 0; c < nc; c++) {
                if (grid[r][c] == 0) {
                    total += dfs(r, c);
                }
            }
        }
        System.out.println(total);
    }

    static long dfs(int r, int c) {
        if (dp[r][c] != -1) {
            return dp[r][c];
        }
        int h = grid[r][c];
        if (h == 9) {
            return dp[r][c] = 1;
        }
        long sum = 0;
        for (int[] d : dirs) {
            int nr2 = r + d[0];
            int nc2 = c + d[1];
            if (nr2 < 0 || nr2 >= nr || nc2 < 0 || nc2 >= nc) continue;
            if (grid[nr2][nc2] == h + 1) {
                sum += dfs(nr2, nc2);
            }
        }
        return dp[r][c] = sum;
    }
}
