
import java.io.File;
import java.io.IOException;
import java.util.*;

class Solution {

    static class State {
        int x, y, d;

        State(int x, int y, int d) {
            this.x = x;
            this.y = y;
            this.d = d;
        }
    }

    static class Node {
        int x, y, d, cost;

        Node(int x, int y, int d, int cost) {
            this.x = x;
            this.y = y;
            this.d = d;
            this.cost = cost;
        }
    }

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(new File("input.txt"));
        List<String> grid = new ArrayList<>();
        while (scanner.hasNextLine()) {
            grid.add(scanner.nextLine());
        }
        scanner.close();

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
                Arrays.fill(dist[i][j], Integer.MAX_VALUE);
            }
        }
        dist[sx][sy][1] = 0;

        PriorityQueue<Node> pq = new PriorityQueue<>(Comparator.comparingInt(node -> node.cost));
        pq.offer(new Node(sx, sy, 1, 0));

        while (!pq.isEmpty()) {
            Node u = pq.poll();
            if (dist[u.x][u.y][u.d] < u.cost) {
                continue;
            }
            if (u.x == ex && u.y == ey) {
                continue;
            }

            for (int ndir : new int[]{(u.d + 1) % 4, (u.d + 3) % 4}) {
                int nc = u.cost + 1000;
                if (nc < dist[u.x][u.y][ndir]) {
                    dist[u.x][u.y][ndir] = nc;
                    pq.offer(new Node(u.x, u.y, ndir, nc));
                }
            }

            int nx = u.x + dx[u.d];
            int ny = u.y + dy[u.d];
            if (nx >= 0 && nx < n && ny >= 0 && ny < m && grid.get(nx).charAt(ny) != '#') {
                int nc = u.cost + 1;
                if (nc < dist[nx][ny][u.d]) {
                    dist[nx][ny][u.d] = nc;
                    pq.offer(new Node(nx, ny, u.d, nc));
                }
            }
        }

        int best = Integer.MAX_VALUE;
        for (int d = 0; d < 4; d++) {
            best = Math.min(best, dist[ex][ey][d]);
        }

        boolean[][] used = new boolean[n][m];
        List<State> rev = new ArrayList<>();
        for (int d = 0; d < 4; d++) {
            if (dist[ex][ey][d] == best) {
                rev.add(new State(ex, ey, d));
            }
        }

        boolean[][][] vis = new boolean[n][m][4];
        for (State s : rev) {
            vis[s.x][s.y][s.d] = true;
        }

        while (!rev.isEmpty()) {
            State u = rev.remove(rev.size() - 1);
            used[u.x][u.y] = true;
            int costU = dist[u.x][u.y][u.d];

            for (int pd : new int[]{(u.d + 1) % 4, (u.d + 3) % 4}) {
                if (dist[u.x][u.y][pd] == costU - 1000) {
                    if (!vis[u.x][u.y][pd]) {
                        vis[u.x][u.y][pd] = true;
                        rev.add(new State(u.x, u.y, pd));
                    }
                }
            }

            int px = u.x - dx[u.d];
            int py = u.y - dy[u.d];
            if (px >= 0 && px < n && py >= 0 && py < m && grid.get(px).charAt(py) != '#') {
                if (dist[px][py][u.d] == costU - 1) {
                    if (!vis[px][py][u.d]) {
                        vis[px][py][u.d] = true;
                        rev.add(new State(px, py, u.d));
                    }
                }
            }
        }

        int cnt = 0;
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                if (used[i][j] && grid.get(i).charAt(j) != '#') {
                    cnt++;
                }
            }
        }
        System.out.println(cnt);
    }
}
