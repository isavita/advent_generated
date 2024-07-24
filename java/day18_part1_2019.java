
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
    }

    static class State {
        Point pos;
        int keys;
        State(Point pos, int keys) {
            this.pos = pos;
            this.keys = keys;
        }
    }

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        List<String> grid = new ArrayList<>();
        Point start = null;
        Map<Character, Integer> keyMap = new HashMap<>();
        int keyCounter = 0;

        String line;
        while ((line = br.readLine()) != null) {
            grid.add(line);
            for (int x = 0; x < line.length(); x++) {
                char c = line.charAt(x);
                if (c == '@') {
                    start = new Point(x, grid.size() - 1);
                } else if (Character.isLowerCase(c)) {
                    keyMap.put(c, keyCounter++);
                }
            }
        }
        br.close();

        System.out.println(findShortestPath(grid, start, keyMap));
    }

    private static int findShortestPath(List<String> grid, Point start, Map<Character, Integer> keyMap) {
        Point[] dirs = {new Point(0, -1), new Point(-1, 0), new Point(0, 1), new Point(1, 0)};
        Set<String> visited = new HashSet<>();
        Queue<State> queue = new LinkedList<>();
        queue.add(new State(start, 0));
        int steps = 0;

        while (!queue.isEmpty()) {
            int size = queue.size();
            for (int i = 0; i < size; i++) {
                State current = queue.poll();
                if (current.keys == (1 << keyMap.size()) - 1) {
                    return steps;
                }

                for (Point d : dirs) {
                    Point next = new Point(current.pos.x + d.x, current.pos.y + d.y);
                    if (next.x >= 0 && next.x < grid.get(0).length() && next.y >= 0 && next.y < grid.size()) {
                        char c = grid.get(next.y).charAt(next.x);
                        if (c != '#' && !(Character.isUpperCase(c) && (current.keys & (1 << keyMap.get(Character.toLowerCase(c)))) == 0)) {
                            State newState = new State(next, current.keys);
                            if (Character.isLowerCase(c)) {
                                newState.keys |= 1 << keyMap.get(c);
                            }
                            String stateKey = next.x + "," + next.y + "," + newState.keys;
                            if (!visited.contains(stateKey)) {
                                visited.add(stateKey);
                                queue.add(newState);
                            }
                        }
                    }
                }
            }
            steps++;
        }
        return -1;
    }
}
