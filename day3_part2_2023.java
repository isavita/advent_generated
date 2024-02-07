
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class solution {
    static class Part {
        int xmin, xmax, y, n;

        public Part(int xmin, int xmax, int y, int n) {
            this.xmin = xmin;
            this.xmax = xmax;
            this.y = y;
            this.n = n;
        }

        boolean valid(Map<Point, Character> grid) {
            for (int x = xmin; x <= xmax; x++) {
                for (Point n : Neighbors8) {
                    char c = grid.getOrDefault(new Point(x + n.x, y + n.y), '.');
                    if (c != '.' && (c < '0' || c > '9')) {
                        return true;
                    }
                }
            }
            return false;
        }
    }

    static class Point {
        int x, y;

        public Point(int x, int y) {
            this.x = x;
            this.y = y;
        }

        Point add(Point other) {
            return new Point(this.x + other.x, this.y + other.y);
        }

        @Override
        public int hashCode() {
            return 31 * x + y;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null || getClass() != obj.getClass())
                return false;
            Point other = (Point) obj;
            return x == other.x && y == other.y;
        }
    }

    static final Point[] Neighbors8 = {new Point(0, 1), new Point(0, -1), new Point(1, 0), new Point(-1, 0),
            new Point(-1, -1), new Point(-1, 1), new Point(1, -1), new Point(1, 1)};

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            StringBuilder inputBuilder = new StringBuilder();
            while ((line = reader.readLine()) != null) {
                inputBuilder.append(line).append("\n");
            }
            reader.close();

            String input = inputBuilder.toString().trim();
            Map<Point, Character> grid = new HashMap<>();
            Set<Part> parts = new HashSet<>();
            Part curr = null;
            String[] lines = input.split("\n");
            for (int y = 0; y < lines.length; y++) {
                for (int x = 0; x < lines[y].length(); x++) {
                    char c = lines[y].charAt(x);
                    grid.put(new Point(x, y), c);
                    if (Character.isDigit(c)) {
                        if (curr == null) {
                            curr = new Part(x, x, y, Character.getNumericValue(c));
                        } else {
                            curr.n = curr.n * 10 + Character.getNumericValue(c);
                            curr.xmax = x;
                        }
                    } else if (curr != null) {
                        parts.add(curr);
                        curr = null;
                    }
                }
                if (curr != null) {
                    parts.add(curr);
                    curr = null;
                }
            }

            Map<Point, Integer> partsGrid = new HashMap<>();
            int i = 0;
            for (Part p : parts) {
                for (int x = p.xmin; x <= p.xmax; x++) {
                    partsGrid.put(new Point(x, p.y), i);
                }
                i++;
            }

            int sum = 0;
            for (Map.Entry<Point, Character> entry : grid.entrySet()) {
                Point p = entry.getKey();
                char c = entry.getValue();
                if (c == '*') {
                    Set<Integer> neighborParts = new HashSet<>();
                    for (Point n : Neighbors8) {
                        int partIndex = partsGrid.getOrDefault(p.add(n), -1);
                        if (partIndex != -1) {
                            neighborParts.add(partIndex);
                        }
                    }
                    if (neighborParts.size() == 2) {
                        int prod = 1;
                        for (int index : neighborParts) {
                            prod *= parts.toArray(new Part[0])[index].n;
                        }
                        sum += prod;
                    }
                }
            }

            System.out.println(sum);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
