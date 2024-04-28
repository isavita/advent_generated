import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

public class Main {

    static class Node implements Comparable<Node> {
        int x, y, risk, distance;

        public Node(int x, int y, int risk) {
            this.x = x;
            this.y = y;
            this.risk = risk;
            this.distance = Integer.MAX_VALUE;
        }

        @Override
        public int compareTo(Node other) {
            return Integer.compare(this.distance, other.distance);
        }
    }

    public static void main(String[] args) throws FileNotFoundException {
        File file = new File("input.txt");
        Scanner scanner = new Scanner(file);

        List<String> lines = new ArrayList<>();
        while (scanner.hasNextLine()) {
            lines.add(scanner.nextLine());
        }

        int rows = lines.size();
        int cols = lines.get(0).length();

        Node[][] nodes = new Node[rows][cols];
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                nodes[i][j] = new Node(i, j, lines.get(i).charAt(j) - '0');
            }
        }

        nodes[0][0].distance = 0;

        PriorityQueue<Node> queue = new PriorityQueue<>();
        queue.add(nodes[0][0]);

        while (!queue.isEmpty()) {
            Node current = queue.poll();
            if (current.distance == Integer.MAX_VALUE) {
                break;
            }

            for (int dx = -1; dx <= 1; dx++) {
                for (int dy = -1; dy <= 1; dy++) {
                    if (Math.abs(dx) == Math.abs(dy)) {
                        continue;
                    }

                    int x = current.x + dx;
                    int y = current.y + dy;

                    if (x < 0 || x >= rows || y < 0 || y >= cols) {
                        continue;
                    }

                    Node neighbor = nodes[x][y];
                    int distance = current.distance + neighbor.risk;

                    if (distance < neighbor.distance) {
                        neighbor.distance = distance;
                        queue.add(neighbor);
                    }
                }
            }
        }

        System.out.println("The lowest total risk of any path from the top left to the bottom right is " + nodes[rows - 1][cols - 1].distance);
    }
}