
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.PriorityQueue;

class Solution {

    static class State {
        int x, y, d, cost;

        State(int x, int y, int d, int cost) {
            this.x = x;
            this.y = y;
            this.d = d;
            this.cost = cost;
        }
    }

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            List<String> grid = new ArrayList<>();
            String line;
            while ((line = br.readLine()) != null) {
                grid.add(line);
            }

            int n = grid.size();
            int m = grid.get(0).length();
            int sx = 0, sy = 0, ex = 0, ey = 0;

            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    if (grid.get(i).charAt(j) == 'S') {
                        sx = i;
                        sy = j;
                    } else if (grid.get(i).charAt(j) == 'E') {
                        ex = i;
                        ey = j;
                    }
                }
            }

            int[] dx = {-1, 0, 1, 0};
            int[] dy = {0, 1, 0, -1};

            int[][][] dist = new int[n][m][4];
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    for (int k = 0; k < 4; k++) {
                        dist[i][j][k] = Integer.MAX_VALUE;
                    }
                }
            }
            dist[sx][sy][1] = 0;

            PriorityQueue<State> pq = new PriorityQueue<>(Comparator.comparingInt(s -> s.cost));
            pq.offer(new State(sx, sy, 1, 0));

            while (!pq.isEmpty()) {
                State u = pq.poll();
                if (dist[u.x][u.y][u.d] < u.cost) {
                    continue;
                }
                if (u.x == ex && u.y == ey) {
                    System.out.println(u.cost);
                    return;
                }

                for (int ndir : new int[]{(u.d + 1) % 4, (u.d + 3) % 4}) {
                    int nc = u.cost + 1000;
                    if (nc < dist[u.x][u.y][ndir]) {
                        dist[u.x][u.y][ndir] = nc;
                        pq.offer(new State(u.x, u.y, ndir, nc));
                    }
                }

                int nx = u.x + dx[u.d];
                int ny = u.y + dy[u.d];
                if (nx >= 0 && nx < n && ny >= 0 && ny < m && grid.get(nx).charAt(ny) != '#') {
                    int nc = u.cost + 1;
                    if (nc < dist[nx][ny][u.d]) {
                        dist[nx][ny][u.d] = nc;
                        pq.offer(new State(nx, ny, u.d, nc));
                    }
                }
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
