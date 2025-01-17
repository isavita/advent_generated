
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class DonutMaze {

    private static class Point {
        int x, y, level;

        public Point(int x, int y, int level) {
            this.x = x;
            this.y = y;
            this.level = level;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Point point = (Point) o;
            return x == point.x && y == point.y && level == point.level;
        }

        @Override
        public int hashCode() {
            return Objects.hash(x, y, level);
        }
    }

    public static void main(String[] args) {
        try {
            char[][] maze = readMaze("input.txt");
            Map<String, List<Point>> portals = findPortals(maze);
            int stepsPart1 = solveMaze(maze, portals, false);
            int stepsPart2 = solveMaze(maze, portals, true);

            System.out.println(stepsPart1);
            System.out.println(stepsPart2);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static char[][] readMaze(String filename) throws IOException {
        List<String> lines = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }
        }
        int rows = lines.size();
        int cols = lines.get(0).length();
        char[][] maze = new char[rows][cols];
        for (int i = 0; i < rows; i++) {
            maze[i] = lines.get(i).toCharArray();
        }
        return maze;
    }

    private static Map<String, List<Point>> findPortals(char[][] maze) {
        Map<String, List<Point>> portals = new HashMap<>();
        int rows = maze.length;
        int cols = maze[0].length;

        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                if (Character.isUpperCase(maze[i][j])) {
                    StringBuilder label = new StringBuilder();
                    label.append(maze[i][j]);
                    Point p = null;

                    if (j + 1 < cols && Character.isUpperCase(maze[i][j + 1])) {
                        label.append(maze[i][j + 1]);
                        if (j + 2 < cols && maze[i][j + 2] == '.') {
                            p = new Point(i, j + 2, 0);
                        } else if (j - 1 >= 0 && maze[i][j - 1] == '.') {
                            p = new Point(i, j - 1, 0);
                        }
                    } else if (i + 1 < rows && Character.isUpperCase(maze[i + 1][j])) {
                        label.append(maze[i + 1][j]);
                        if (i + 2 < rows && maze[i + 2][j] == '.') {
                            p = new Point(i + 2, j, 0);
                        } else if (i - 1 >= 0 && maze[i - 1][j] == '.') {
                            p = new Point(i - 1, j, 0);
                        }
                    }

                    if (p != null) {
                        portals.computeIfAbsent(label.toString(), k -> new ArrayList<>()).add(p);
                    }
                }
            }
        }
        return portals;
    }

    private static int solveMaze(char[][] maze, Map<String, List<Point>> portals, boolean recursive) {
        Point start = portals.get("AA").get(0);
        Point end = portals.get("ZZ").get(0);
        Queue<Point> queue = new LinkedList<>();
        queue.offer(new Point(start.x, start.y, 0));
        Map<Point, Integer> dist = new HashMap<>();
        dist.put(new Point(start.x, start.y, 0), 0);

        int[] dx = {0, 0, 1, -1};
        int[] dy = {1, -1, 0, 0};

        while (!queue.isEmpty()) {
            Point curr = queue.poll();

            if (curr.x == end.x && curr.y == end.y && (!recursive || curr.level == 0)) {
                return dist.get(curr);
            }

            for (int i = 0; i < 4; i++) {
                int nx = curr.x + dx[i];
                int ny = curr.y + dy[i];
                Point next = new Point(nx, ny, curr.level);

                if (nx >= 0 && nx < maze.length && ny >= 0 && ny < maze[0].length && maze[nx][ny] == '.') {
                    if (!dist.containsKey(next)) {
                        dist.put(next, dist.get(curr) + 1);
                        queue.offer(next);
                    }
                }
            }

            for (Map.Entry<String, List<Point>> entry : portals.entrySet()) {
                List<Point> points = entry.getValue();
                if (points.size() == 2) {
                    Point p1 = points.get(0);
                    Point p2 = points.get(1);
                    Point portal = null;
                    Point other = null;
                    int newLevel = curr.level;

                    if (curr.x == p1.x && curr.y == p1.y) {
                        portal = p1;
                        other = p2;
                    } else if (curr.x == p2.x && curr.y == p2.y) {
                        portal = p2;
                        other = p1;
                    }

                    if (portal != null) {
                        if (recursive) {
                            if (isOuter(portal, maze.length, maze[0].length)) {
                                newLevel--;
                            } else {
                                newLevel++;
                            }
                            if (newLevel < 0 || (newLevel == 0 && (entry.getKey().equals("AA") || entry.getKey().equals("ZZ"))))
                                continue;
                        }
                        Point next = new Point(other.x, other.y, newLevel);
                        if (!dist.containsKey(next)) {
                            dist.put(next, dist.get(curr) + 1);
                            queue.offer(next);
                        }
                    }
                }
            }
        }
        return -1;
    }

    private static boolean isOuter(Point p, int rows, int cols) {
        return p.x <= 2 || p.x >= rows - 3 || p.y <= 2 || p.y >= cols - 3;
    }
}
