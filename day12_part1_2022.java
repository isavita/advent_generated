
import java.io.File;
import java.io.FileNotFoundException;
import java.util.PriorityQueue;
import java.util.Scanner;

public class Solution {

    public static void main(String[] args) {
        char[][] grid = new char[1000][1000];
        int[][] dists = new int[1000][1000];
        Point start = new Point(0, 0);
        Point end = new Point(0, 0);
        Point[] as = new Point[1000];
        int y = 0;
        try {
            Scanner scanner = new Scanner(new File("input.txt"));
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                for (int x = 0; x < line.length(); x++) {
                    char b = line.charAt(x);
                    Point p = new Point(x, y);
                    grid[p.x][p.y] = b;
                    if (b == 'S') {
                        start = p;
                    } else if (b == 'E') {
                        end = p;
                    } else if (b == 'a') {
                        as[y] = p;
                    }
                }
                y++;
            }
            grid[start.x][start.y] = 'a';
            grid[end.x][end.y] = 'z';

            dists = djikstra(grid, end);

            int l = dists[start.x][start.y];
            System.out.println(l);

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    static class Point {
        int x;
        int y;

        Point(int x, int y) {
            this.x = x;
            this.y = y;
        }

        Point add(Point other) {
            return new Point(x + other.x, y + other.y);
        }
    }

    static class Item {
        Point obj;
        int priority;

        Item(Point obj, int priority) {
            this.obj = obj;
            this.priority = priority;
        }
    }

    static Point[] Neighbors4 = {new Point(0, 1), new Point(0, -1), new Point(1, 0), new Point(-1, 0)};

    static int[][] djikstra(char[][] grid, Point end) {
        PriorityQueue<Item> pq = new PriorityQueue<>((a, b) -> b.priority - a.priority);
        int[][] dist = new int[1000][1000];
        pq.add(new Item(end, 0));
        dist[end.x][end.y] = 0;
        while (!pq.isEmpty()) {
            Point curr = pq.poll().obj;
            for (Point n : Neighbors4) {
                Point next = curr.add(n);
                if (next.x < 0 || next.x >= 1000 || next.y < 0 || next.y >= 1000 || grid[next.x][next.y] == '\u0000') {
                    continue;
                }
                if (grid[curr.x][curr.y] - grid[next.x][next.y] > 1) {
                    continue;
                }
                int nextdist = dist[curr.x][curr.y] + 1;
                if (dist[next.x][next.y] == 0 || nextdist < dist[next.x][next.y]) {
                    dist[next.x][next.y] = nextdist;
                    pq.add(new Item(next, nextdist));
                }
            }
        }
        return dist;
    }
}
