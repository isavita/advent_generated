
import java.awt.Point;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class Main {
    static Map<Point, Boolean> grid = new HashMap<>();
    static final Point DOWN = new Point(0, 1);
    static final Point LEFT = new Point(-1, 0);
    static final Point RIGHT = new Point(1, 0);

    public static void main(String[] args) throws IOException {
        readInput("input.txt");
        System.out.println(fill());
    }

    static void readInput(String filename) throws IOException {
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.split(" -> ");
                Point[] pts = new Point[parts.length];
                for (int i = 0; i < parts.length; i++) {
                    String[] coords = parts[i].split(",");
                    pts[i] = new Point(Integer.parseInt(coords[0]), Integer.parseInt(coords[1]));
                }
                for (int i = 0; i < pts.length - 1; i++) {
                    drawLine(pts[i], pts[i + 1]);
                }
            }
        }
    }

    static void drawLine(Point p1, Point p2) {
        if (p1.x == p2.x) {
            for (int y = Math.min(p1.y, p2.y); y <= Math.max(p1.y, p2.y); y++) {
                grid.put(new Point(p1.x, y), true);
            }
        } else {
            for (int x = Math.min(p1.x, p2.x); x <= Math.max(p1.x, p2.x); x++) {
                grid.put(new Point(x, p1.y), true);
            }
        }
    }

    static int fill() {
        int floor = grid.keySet().stream().mapToInt(p -> p.y).max().orElse(0) + 1;
        int sands = 0, firstFloorTouch = 0;

        while (!grid.containsKey(new Point(500, 0))) {
            Point sand = new Point(500, 0);
            boolean settled = false;

            while (!settled) {
                if (sand.y == floor) {
                    if (firstFloorTouch == 0) firstFloorTouch = sands;
                    grid.put(sand, true);
                    settled = true;
                } else {
                    Point nextSand = next(sand);
                    if (nextSand.equals(sand)) {
                        grid.put(sand, true);
                        settled = true;
                    } else {
                        sand = nextSand;
                    }
                }
            }
            sands++;
        }
        return firstFloorTouch;
    }

    static Point next(Point sand) {
        Point[] directions = {new Point(sand.x + DOWN.x, sand.y + DOWN.y),
                              new Point(sand.x + DOWN.x + LEFT.x, sand.y + DOWN.y + LEFT.y),
                              new Point(sand.x + DOWN.x + RIGHT.x, sand.y + DOWN.y + RIGHT.y)};
        for (Point n : directions) {
            if (!grid.containsKey(n)) return n;
        }
        return sand;
    }
}
