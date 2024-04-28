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
            Position that = (Position) obj;
            return x == that.x && y == that.y;
        }
    }

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        Map<Position, Integer> grid = new HashMap<>();
        int startX = 0, startY = 0;
        int yCoord = 0;
        while (true) {
            String line = br.readLine();
            if (line == null) break;
            for (int x = 0; x < line.length(); x++) {
                if (line.charAt(x) == '#') {
                    grid.put(new Position(x, yCoord), 2);
                }
            }
            startX = line.length() / 2;
            startY = yCoord / 2;
            yCoord++;
        }
        br.close();

        int[] dx = {0, 1, 0, -1};
        int[] dy = {-1, 0, 1, 0};

        int xCoord = startX, yCoordNew = startY, dir = 0;
        int infectedCount = 0;

        for (int i = 0; i < 10000000; i++) {
            Position pos = new Position(xCoord, yCoordNew);
            int state = grid.getOrDefault(pos, 0);
            switch (state) {
                case 0:
                    dir = (dir - 1 + 4) % 4;
                    grid.put(pos, 1);
                    break;
                case 1:
                    grid.put(pos, 2);
                    infectedCount++;
                    break;
                case 2:
                    dir = (dir + 1) % 4;
                    grid.put(pos, 3);
                    break;
                case 3:
                    dir = (dir + 2) % 4;
                    grid.put(pos, 0);
                    break;
            }
            xCoord += dx[dir];
            yCoordNew += dy[dir];
        }

        System.out.println(infectedCount);
    }
}