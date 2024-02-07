
import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class solution {
    static class Point {
        int x, y;

        Point(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public int hashCode() {
            return 31 * x + y;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) return true;
            if (obj == null || getClass() != obj.getClass()) return false;
            Point point = (Point) obj;
            return x == point.x && y == point.y;
        }
    }

    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            Point head = new Point(0, 0);
            Point tail = new Point(0, 0);
            Map<Point, Boolean> visited = new HashMap<>();
            visited.put(tail, true);

            while (scanner.hasNextLine()) {
                String[] line = scanner.nextLine().split(" ");
                String dir = line[0];
                int numSteps = Integer.parseInt(line[1]);

                for (int i = 0; i < numSteps; i++) {
                    switch (dir) {
                        case "R":
                            head.x++;
                            break;
                        case "L":
                            head.x--;
                            break;
                        case "U":
                            head.y++;
                            break;
                        case "D":
                            head.y--;
                            break;
                    }

                    if (Math.abs(head.x - tail.x) > 1 || Math.abs(head.y - tail.y) > 1) {
                        if (head.x != tail.x && head.y != tail.y) {
                            if (head.x > tail.x) {
                                tail.x++;
                            } else {
                                tail.x--;
                            }
                            if (head.y > tail.y) {
                                tail.y++;
                            } else {
                                tail.y--;
                            }
                        } else {
                            if (head.x > tail.x) {
                                tail.x++;
                            } else if (head.x < tail.x) {
                                tail.x--;
                            }
                            if (head.y > tail.y) {
                                tail.y++;
                            } else if (head.y < tail.y) {
                                tail.y--;
                            }
                        }
                    }

                    visited.put(new Point(tail.x, tail.y), true);
                }
            }

            System.out.println(visited.size());
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}
