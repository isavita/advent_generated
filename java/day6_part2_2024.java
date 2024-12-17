
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

class Solution {

    static class State {
        int x, y, dir;

        State(int x, int y, int dir) {
            this.x = x;
            this.y = y;
            this.dir = dir;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            State state = (State) o;
            return x == state.x && y == state.y && dir == state.dir;
        }

        @Override
        public int hashCode() {
            int result = x;
            result = 31 * result + y;
            result = 31 * result + dir;
            return result;
        }
    }

    public static void main(String[] args) throws Exception {
        Scanner scanner = new Scanner(new File("input.txt"));
        List<char[]> grid = new ArrayList<>();
        while (scanner.hasNextLine()) {
            grid.add(scanner.nextLine().toCharArray());
        }
        scanner.close();

        int h = grid.size();
        int w = grid.get(0).length;

        int startX = 0, startY = 0, startDir = 0;
        for (int i = 0; i < h; i++) {
            for (int j = 0; j < w; j++) {
                switch (grid.get(i)[j]) {
                    case '^':
                        startX = j;
                        startY = i;
                        startDir = 0;
                        break;
                    case '>':
                        startX = j;
                        startY = i;
                        startDir = 1;
                        break;
                    case 'v':
                        startX = j;
                        startY = i;
                        startDir = 2;
                        break;
                    case '<':
                        startX = j;
                        startY = i;
                        startDir = 3;
                        break;
                }
            }
        }
        grid.get(startY)[startX] = '.';

        int canLoop = 0;
        for (int y = 0; y < h; y++) {
            for (int x = 0; x < w; x++) {
                if (x == startX && y == startY) continue;
                if (grid.get(y)[x] != '.') continue;
                grid.get(y)[x] = '#';
                if (loops(grid, startX, startY, startDir)) canLoop++;
                grid.get(y)[x] = '.';
            }
        }
        System.out.println(canLoop);
    }

    static boolean loops(List<char[]> grid, int sx, int sy, int sdir) {
        int h = grid.size();
        int w = grid.get(0).length;
        int[][] dirs = {{0, -1}, {1, 0}, {0, 1}, {-1, 0}};
        int x = sx, y = sy, dir = sdir;
        Map<State, Boolean> seen = new HashMap<>();
        for (int step = 0; step < 2000000; step++) {
            State st = new State(x, y, dir);
            if (seen.containsKey(st)) return true;
            seen.put(st, true);
            int dx = dirs[dir][0], dy = dirs[dir][1];
            int nx = x + dx, ny = y + dy;
            if (nx < 0 || nx >= w || ny < 0 || ny >= h) return false;
            if (grid.get(ny)[nx] == '#') {
                dir = (dir + 1) % 4;
                continue;
            }
            x = nx;
            y = ny;
        }
        return false;
    }
}
