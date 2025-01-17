
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class LavaductLagoon {

    public static void main(String[] args) {
        try {
            List<String> lines = readInput("input.txt");
            long area = calculateArea(lines);
            System.out.println(area);
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static List<String> readInput(String filename) throws IOException {
        List<String> lines = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }
        }
        return lines;
    }

    private static long calculateArea(List<String> instructions) {
        List<Point> vertices = new ArrayList<>();
        vertices.add(new Point(0, 0));
        long perimeter = 0;

        for (String instruction : instructions) {
            String[] parts = instruction.split(" ");
            char direction = parts[0].charAt(0);
            int steps = Integer.parseInt(parts[1]);
            perimeter += steps;

            Point lastVertex = vertices.get(vertices.size() - 1);
            long nextX = lastVertex.x;
            long nextY = lastVertex.y;

            switch (direction) {
                case 'U':
                    nextY -= steps;
                    break;
                case 'D':
                    nextY += steps;
                    break;
                case 'L':
                    nextX -= steps;
                    break;
                case 'R':
                    nextX += steps;
                    break;
            }
            vertices.add(new Point(nextX, nextY));
        }

        // Shoelace formula to calculate area
        long area = 0;
        for (int i = 0; i < vertices.size() - 1; i++) {
            area += (vertices.get(i).x * vertices.get(i + 1).y) - (vertices.get(i + 1).x * vertices.get(i).y);
        }
        area = Math.abs(area) / 2;

        // Pick's theorem: A = I + B/2 - 1, where A is area, I is interior points, B is boundary points
        // We want to find I + B, which is the total volume
        // I + B = A + B/2 + 1
        return area + perimeter / 2 + 1;
    }

    static class Point {
        long x;
        long y;

        public Point(long x, long y) {
            this.x = x;
            this.y = y;
        }
    }
}
