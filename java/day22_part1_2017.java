import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class Main {
    static class Position {
        int x, y;
        Position(int x, int y) {
            this.x = x;
            this.y = y;
        }
        @Override
        public int hashCode() {
            return x * 31 + y;
        }
        @Override
        public boolean equals(Object obj) {
            if (this == obj) return true;
            if (obj == null || getClass() != obj.getClass()) return false;
            Position p = (Position) obj;
            return x == p.x && y == p.y;
        }
    }

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        Map<Position, Boolean> grid = new HashMap<>();
        int startX = 0, startY = 0;
        int yCoord = 0;
        while (true) {
            String line = br.readLine();
            if (line == null) break;
            for (int x = 0; x < line.length(); x++) {
                if (line.charAt(x) == '#') {
                    grid.put(new Position(x, yCoord), true);
                }
            }
            startX = line.length() / 2;
            startY = yCoord / 2;
            yCoord++;
        }

        int[] dx = {0, 1, 0, -1};
        int[] dy = {-1, 0, 1, 0};
        int xCoord = startX, yCoordCurr = startY, dir = 0;
        int infectedCount = 0;

        for (int i = 0; i < 10000; i++) {
            Position pos = new Position(xCoord, yCoordCurr);
            if (grid.containsKey(pos)) {
                dir = (dir + 1) % 4;
                grid.remove(pos);
            } else {
                dir = (dir - 1 + 4) % 4;
                grid.put(pos, true);
                infectedCount++;
            }
            xCoord += dx[dir];
            yCoordCurr += dy[dir];
        }

        System.out.println(infectedCount);
    }
}