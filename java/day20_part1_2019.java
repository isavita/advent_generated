
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class DonutMaze {

    static class Point {
        int x, y;

        public Point(int x, int y) {
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

    static class State {
        Point point;
        int steps;

        public State(Point point, int steps) {
            this.point = point;
            this.steps = steps;
        }
    }

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            List<String> lines = new ArrayList<>();
            String line;
            while ((line = br.readLine()) != null) {
                lines.add(line);
            }

            char[][] maze = new char[lines.size()][lines.get(0).length()];
            for (int i = 0; i < lines.size(); i++) {
                maze[i] = lines.get(i).toCharArray();
            }

            Map<String, List<Point>> portals = findPortals(maze);
            Point start = findStart(maze, portals);
            Point end = findEnd(maze, portals);

            int minSteps = bfs(maze, start, end, portals);
            System.out.println(minSteps);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static Map<String, List<Point>> findPortals(char[][] maze) {
        Map<String, List<Point>> portals = new HashMap<>();
        int rows = maze.length;
        int cols = maze[0].length;

        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                if (Character.isUpperCase(maze[r][c])) {
                    String portalName;
                    Point portalPoint;
                    if (r + 1 < rows && Character.isUpperCase(maze[r + 1][c])) {
                        portalName = "" + maze[r][c] + maze[r + 1][c];
                        if (r + 2 < rows && maze[r + 2][c] == '.') {
                            portalPoint = new Point(c, r + 2);
                        } else {
                            portalPoint = new Point(c, r - 1);
                        }
                    } else if (c + 1 < cols && Character.isUpperCase(maze[r][c + 1])) {
                        portalName = "" + maze[r][c] + maze[r][c + 1];
                        if (c + 2 < cols && maze[r][c + 2] == '.') {
                            portalPoint = new Point(c + 2, r);
                        } else {
                            portalPoint = new Point(c - 1, r);
                        }
                    } else {
                        continue;
                    }
                    portals.computeIfAbsent(portalName, k -> new ArrayList<>()).add(portalPoint);
                }
            }
        }
        return portals;
    }

    private static Point findStart(char[][] maze, Map<String, List<Point>> portals) {
        return portals.get("AA").get(0);
    }

    private static Point findEnd(char[][] maze, Map<String, List<Point>> portals) {
        return portals.get("ZZ").get(0);
    }

    private static int bfs(char[][] maze, Point start, Point end, Map<String, List<Point>> portals) {
        int rows = maze.length;
        int cols = maze[0].length;
        Queue<State> queue = new LinkedList<>();
        Set<Point> visited = new HashSet<>();

        queue.add(new State(start, 0));
        visited.add(start);

        int[] dr = {-1, 1, 0, 0};
        int[] dc = {0, 0, -1, 1};

        while (!queue.isEmpty()) {
            State current = queue.poll();
            Point currentPoint = current.point;
            int currentSteps = current.steps;

            if (currentPoint.equals(end)) {
                return currentSteps;
            }

            for (int i = 0; i < 4; i++) {
                int newRow = currentPoint.y + dr[i];
                int newCol = currentPoint.x + dc[i];

                if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols && maze[newRow][newCol] == '.') {
                    Point nextPoint = new Point(newCol, newRow);
                    if (visited.add(nextPoint)) {
                        queue.add(new State(nextPoint, currentSteps + 1));
                    }
                }
            }

            for (Map.Entry<String, List<Point>> entry : portals.entrySet()) {
                String portalName = entry.getKey();
                List<Point> portalPoints = entry.getValue();
                if (!portalName.equals("AA") && !portalName.equals("ZZ") && portalPoints.contains(currentPoint)) {
                    Point otherPortal = portalPoints.get(0).equals(currentPoint) ? portalPoints.get(1) : portalPoints.get(0);
                    if (visited.add(otherPortal)) {
                        queue.add(new State(otherPortal, currentSteps + 1));
                    }
                }
            }
        }
        return -1;
    }
}
