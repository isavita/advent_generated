import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {
    static final int gridSize = 1000;
    static boolean[][] grid = new boolean[gridSize][gridSize];

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                processInstruction(line);
            }
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
            return;
        }
        System.out.println(countLights());
    }

    static void processInstruction(String instruction) {
        String[] parts = instruction.split(" ");
        int startX, startY, endX, endY;
        String[] coords = parts[parts.length - 3].split(",");
        startX = Integer.parseInt(coords[0]);
        startY = Integer.parseInt(coords[1]);
        coords = parts[parts.length - 1].split(",");
        endX = Integer.parseInt(coords[0]);
        endY = Integer.parseInt(coords[1]);

        for (int x = startX; x <= endX; x++) {
            for (int y = startY; y <= endY; y++) {
                if (instruction.startsWith("turn on")) {
                    grid[x][y] = true;
                } else if (instruction.startsWith("turn off")) {
                    grid[x][y] = false;
                } else if (instruction.startsWith("toggle")) {
                    grid[x][y] = !grid[x][y];
                }
            }
        }
    }

    static int countLights() {
        int count = 0;
        for (boolean[] row : grid) {
            for (boolean light : row) {
                if (light) {
                    count++;
                }
            }
        }
        return count;
    }
}