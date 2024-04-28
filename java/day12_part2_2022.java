import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
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

    static class Item implements Comparable<Item> {
        Point obj;
        int priority;

        Item(Point obj, int priority) {
            this.obj = obj;
            this.priority = priority;
        }

        @Override
        public int compareTo(Item other) {
            return Integer.compare(other.priority, this.priority);
        }
    }

    static Map<Point, Byte> grid = new HashMap<>();
    static Point start, end;
    static List<Point> as = new ArrayList<>();

    public static void main(String[] args) throws IOException {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            int y = 0;
            while ((line = br.readLine()) != null) {
                for (int x = 0; x < line.length(); x++) {
                    Point p = new Point(x, y);
                    byte b = (byte) line.charAt(x);
                    grid.put(p, b);
                    if (b == 'S') {
                        start = p;
                    } else if (b == 'E') {
                        end = p;
                    } else if (b == 'a') {
                        as.add(p);
                    }
                }
                y++;
            }
        }
        grid.put(start, (byte) 'a');
        grid.put(end, (byte) 'z');

        Map<Point, Integer> dists = djikstra(grid, end);

        int l = dists.getOrDefault(start, Integer.MAX_VALUE);

        for (Point a : as) {
            l = Math.min(l, dists.getOrDefault(a, Integer.MAX_VALUE));
        }
        System.out.println(l);
    }

    static final List<Point> Neighbors4 = Arrays.asList(
            new Point(0, 1), new Point(0, -1), new Point(1, 0), new Point(-1, 0)
    );

    static Map<Point, Integer> djikstra(Map<Point, Byte> grid, Point end) {
        PriorityQueue<Item> pq = new PriorityQueue<>();
        Map<Point, Integer> dist = new HashMap<>();
        dist.put(end, 0);
        pq.add(new Item(end, 0));
        while (!pq.isEmpty()) {
            Item curr = pq.poll();
            for (Point n : Neighbors4) {
                Point next = new Point(curr.obj.x + n.x, curr.obj.y + n.y);
                if (!grid.containsKey(next)) {
                    continue;
                }
                if (grid.get(curr.obj) - grid.get(next) > 1) {
                    continue;
                }
                int nextdist = dist.get(curr.obj) + 1;
                if (!dist.containsKey(next) || nextdist < dist.get(next)) {
                    dist.put(next, nextdist);
                    pq.add(new Item(next, nextdist));
                }
            }
        }
        return dist;
    }
}