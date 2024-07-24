
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Main {
    static class Point {
        int x, y;
        Point(int x, int y) { this.x = x; this.y = y; }
        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof Point)) return false;
            Point p = (Point) o;
            return x == p.x && y == p.y;
        }
        @Override
        public int hashCode() { return Objects.hash(x, y); }
    }

    static class DoorMap {
        Map<Point, Map<Point, Boolean>> map = new HashMap<>();
        void addDoor(Point from, Point to) {
            map.computeIfAbsent(from, k -> new HashMap<>()).put(to, true);
        }
        Map<Point, Boolean> getDoors(Point p) { return map.getOrDefault(p, Collections.emptyMap()); }
    }

    public static void main(String[] args) throws IOException {
        String regex = new String(Files.readAllBytes(Paths.get("input.txt"))).trim();
        DoorMap dm = buildMap(regex.substring(1, regex.length() - 1));
        System.out.println(findFurthestRoom(dm));
    }

    static DoorMap buildMap(String regex) {
        DoorMap dm = new DoorMap();
        Stack<Point> stack = new Stack<>();
        Point cp = new Point(0, 0);
        for (char c : regex.toCharArray()) {
            switch (c) {
                case '(' -> stack.push(cp);
                case '|' -> cp = stack.peek();
                case ')' -> cp = stack.pop();
                default -> {
                    Point np = move(cp, c);
                    dm.addDoor(cp, np);
                    cp = np;
                }
            }
        }
        return dm;
    }

    static Point move(Point p, char dir) {
        return switch (dir) {
            case 'N' -> new Point(p.x, p.y - 1);
            case 'S' -> new Point(p.x, p.y + 1);
            case 'E' -> new Point(p.x + 1, p.y);
            case 'W' -> new Point(p.x - 1, p.y);
            default -> p;
        };
    }

    static int findFurthestRoom(DoorMap dm) {
        Map<Point, Integer> visited = new HashMap<>();
        Queue<Point> queue = new LinkedList<>();
        queue.add(new Point(0, 0));
        visited.put(new Point(0, 0), 0);
        int maxDoors = 0;

        while (!queue.isEmpty()) {
            Point p = queue.poll();
            for (Point np : dm.getDoors(p).keySet()) {
                if (!visited.containsKey(np)) {
                    visited.put(np, visited.get(p) + 1);
                    maxDoors = Math.max(maxDoors, visited.get(np));
                    queue.add(np);
                }
            }
        }
        return maxDoors;
    }
}
