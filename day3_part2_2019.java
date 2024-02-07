
import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class solution {
    static class Point {
        int X, Y;

        Point(int x, int y) {
            X = x;
            Y = y;
        }

        @Override
        public int hashCode() {
            return X * 31 + Y;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null || getClass() != obj.getClass()) {
                return false;
            }
            Point other = (Point) obj;
            return X == other.X && Y == other.Y;
        }
    }

    public static void main(String[] args) {
        File file = new File("input.txt");
        try {
            Scanner scanner = new Scanner(file);
            String line1 = scanner.nextLine();
            String line2 = scanner.nextLine();
            Map<Point, Integer> wire1 = getPointsWithSteps(line1);
            Map<Point, Integer> wire2 = getPointsWithSteps(line2);

            int minSteps = Integer.MAX_VALUE;
            for (Point p : wire1.keySet()) {
                if (wire2.containsKey(p)) {
                    int totalSteps = wire1.get(p) + wire2.get(p);
                    if (totalSteps < minSteps) {
                        minSteps = totalSteps;
                    }
                }
            }

            System.out.println(minSteps);
            scanner.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static Map<Point, Integer> getPointsWithSteps(String path) {
        Map<Point, Integer> points = new HashMap<>();
        Point current = new Point(0, 0);
        int steps = 0;
        String[] moves = path.split(",");
        for (String move : moves) {
            char dir = move.charAt(0);
            int dist = Integer.parseInt(move.substring(1));
            for (int i = 0; i < dist; i++) {
                steps++;
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
                if (!points.containsKey(current)) {
                    points.put(new Point(current.X, current.Y), steps);
                }
            }
        }
        return points;
    }
}
