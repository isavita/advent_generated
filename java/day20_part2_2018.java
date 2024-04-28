import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

public class Main {

    static class Point {
        int x, y;

        Point(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Point point = (Point) o;
            return x == point.x && y == point.y;
        }

        @Override
        public int hashCode() {
            return Objects.hash(x, y);
        }
    }

    static class DoorMap extends HashMap<Point, HashMap<Point, Boolean>> {}

    public static void main(String[] args) throws FileNotFoundException {
        File file = new File("input.txt");
        Scanner scanner = new Scanner(file);
        String regex = scanner.next();
        DoorMap dm = buildMap(regex.substring(1, regex.length() - 1));
        int rooms = countRooms(dm, 1000);
        System.out.println(rooms);
    }

    static DoorMap buildMap(String regex) {
        DoorMap dm = new DoorMap();
        Stack<Point> stack = new Stack<>();
        Point cp = new Point(0, 0);
        for (char c : regex.toCharArray()) {
            if (c == '(') {
                stack.push(cp);
            } else if (c == '|') {
                cp = stack.peek();
            } else if (c == ')') {
                cp = stack.pop();
            } else {
                Point np = move(cp, c);
                dm.computeIfAbsent(cp, k -> new HashMap<>()).put(np, true);
                cp = np;
            }
        }
        return dm;
    }

    static Point move(Point p, char dir) {
        switch (dir) {
            case 'N':
                return new Point(p.x, p.y - 1);
            case 'S':
                return new Point(p.x, p.y + 1);
            case 'E':
                return new Point(p.x + 1, p.y);
            case 'W':
                return new Point(p.x - 1, p.y);
            default:
                return p;
        }
    }

    static int countRooms(DoorMap dm, int minDoors) {
        Map<Point, Integer> visited = new HashMap<>();
        Queue<Point> queue = new LinkedList<>();
        queue.add(new Point(0, 0));
        int roomCount = 0;

        while (!queue.isEmpty()) {
            Point p = queue.poll();
            for (Map.Entry<Point, Boolean> entry : dm.getOrDefault(p, new HashMap<>()).entrySet()) {
                Point np = entry.getKey();
                if (!visited.containsKey(np)) {
                    visited.put(np, visited.getOrDefault(p, 0) + 1);
                    if (visited.get(np) >= minDoors) {
                        roomCount++;
                    }
                    queue.add(np);
                }
            }
        }
        return roomCount;
    }
}