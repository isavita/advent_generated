import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class TransparentOrigami {
    static class Point {
        int x, y;

        public Point(int x, int y) {
            this.x = x;
            this.y = y;
        }
    }

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            List<Point> points = new ArrayList<>();
            List<String> folds = new ArrayList<>();

            String line;
            while (!(line = br.readLine()).isEmpty()) {
                String[] coord = line.split(",");
                points.add(new Point(Integer.parseInt(coord[0]), Integer.parseInt(coord[1])));
            }

            while ((line = br.readLine()) != null) {
                folds.add(line.substring(11));
            }

            for (String fold : folds) {
                if (fold.startsWith("x=")) {
                    int x = Integer.parseInt(fold.substring(2));
                    for (Point point : points) {
                        if (point.x > x) {
                            point.x = 2 * x - point.x;
                        }
                    }
                } else {
                    int y = Integer.parseInt(fold.substring(2));
                    for (Point point : points) {
                        if (point.y > y) {
                            point.y = 2 * y - point.y;
                        }
                    }
                }
                Set<Point> set = new HashSet<>(points);
                points.clear();
                points.addAll(set);
            }

            System.out.println("Part 1: " + points.size());

            int maxX = 0, maxY = 0;
            for (Point point : points) {
                maxX = Math.max(maxX, point.x);
                maxY = Math.max(maxY, point.y);
            }

            char[][] grid = new char[maxX + 1][maxY + 1];
            for (int i = 0; i <= maxX; i++) {
                for (int j = 0; j <= maxY; j++) {
                    grid[i][j] = '.';
                }
            }

            for (Point point : points) {
                grid[point.x][point.y] = '#';
            }

            for (int i = 0; i <= maxY; i++) {
                for (int j = 0; j <= maxX; j++) {
                    System.out.print(grid[j][i]);
                }
                System.out.println();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}