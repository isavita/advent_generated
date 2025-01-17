
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class LavaductLagoon {

    public static void main(String[] args) {
        try {
            List<Instruction> instructions = readInstructions("input.txt");
            long part1Result = calculateLagoonSize(instructions, false);
            System.out.println("Part 1: " + part1Result);

            long part2Result = calculateLagoonSize(instructions, true);
            System.out.println("Part 2: " + part2Result);
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static List<Instruction> readInstructions(String filename) throws IOException {
        List<Instruction> instructions = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                String[] parts = line.split(" ");
                char direction = parts[0].charAt(0);
                int distance = Integer.parseInt(parts[1]);
                String colorCode = parts[2].substring(2, parts[2].length() - 1);
                instructions.add(new Instruction(direction, distance, colorCode));
            }
        }
        return instructions;
    }

    private static long calculateLagoonSize(List<Instruction> instructions, boolean useColorCode) {
        List<Point> vertices = new ArrayList<>();
        long perimeter = 0;
        long x = 0, y = 0;
        vertices.add(new Point(x, y));

        for (Instruction instruction : instructions) {
            int distance;
            char direction;

            if (useColorCode) {
                distance = Integer.parseInt(instruction.colorCode.substring(0, 5), 16);
                direction = "RDLU".charAt(Integer.parseInt(instruction.colorCode.substring(5)));
            } else {
                distance = instruction.distance;
                direction = instruction.direction;
            }

            perimeter += distance;

            switch (direction) {
                case 'R':
                    x += distance;
                    break;
                case 'D':
                    y += distance;
                    break;
                case 'L':
                    x -= distance;
                    break;
                case 'U':
                    y -= distance;
                    break;
            }
            vertices.add(new Point(x, y));
        }

        return calculateArea(vertices, perimeter);
    }

    private static long calculateArea(List<Point> vertices, long perimeter) {
        long area = 0;
        for (int i = 0; i < vertices.size() - 1; i++) {
            area += (vertices.get(i).x * vertices.get(i + 1).y - vertices.get(i + 1).x * vertices.get(i).y);
        }
        area = Math.abs(area) / 2;
        return area + perimeter / 2 + 1;
    }

    static class Instruction {
        char direction;
        int distance;
        String colorCode;

        public Instruction(char direction, int distance, String colorCode) {
            this.direction = direction;
            this.distance = distance;
            this.colorCode = colorCode;
        }
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
