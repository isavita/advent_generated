
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;

public class solution {
    static class Point {
        int X, Y;

        public Point(int x, int y) {
            X = x;
            Y = y;
        }

        @Override
        public int hashCode() {
            return X * 31 + Y;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof Point) {
                Point p = (Point) obj;
                return X == p.X && Y == p.Y;
            }
            return false;
        }
    }

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String[] lines = reader.lines().toArray(String[]::new);
            HashMap<Point, Boolean> wire1 = getPoints(lines[0]);
            HashMap<Point, Boolean> wire2 = getPoints(lines[1]);

            HashMap<Point, Boolean> intersections = new HashMap<>();
            for (Point p : wire1.keySet()) {
                if (wire2.containsKey(p)) {
                    intersections.put(p, true);
                }
            }

            int minDistance = Integer.MAX_VALUE;
            for (Point p : intersections.keySet()) {
                int distance = Math.abs(p.X) + Math.abs(p.Y);
                if (distance < minDistance) {
                    minDistance = distance;
                }
            }

            System.out.println(minDistance);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static HashMap<Point, Boolean> getPoints(String path) {
        HashMap<Point, Boolean> points = new HashMap<>();
        Point current = new Point(0, 0);
        String[] moves = path.split(",");
        for (String move : moves) {
            char dir = move.charAt(0);
            int steps = Integer.parseInt(move.substring(1));
            for (int i = 0; i < steps; i++) {
                switch (dir) {
                    case 'U':
                        current.Y++;
                        break;
                    case 'D':
                        current.Y--;
                        break;
                    case 'L':
                        current.X--;
                        break;
                    case 'R':
                        current.X++;
                        break;
                }
                points.put(new Point(current.X, current.Y), true);
            }
        }
        return points;
    }
}
