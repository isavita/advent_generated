
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class solution {
    private static final int gridSize = 1000;

    public static void main(String[] args) {
        int[][] grid = new int[gridSize][gridSize];

        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            while (scanner.hasNextLine()) {
                String instruction = scanner.nextLine();
                processInstruction(instruction, grid);
            }
            scanner.close();
        } catch (FileNotFoundException e) {
            System.out.println("Error opening file: " + e);
            return;
        }

        System.out.println(totalBrightness(grid));
    }

    private static void processInstruction(String instruction, int[][] grid) {
        String[] parts = instruction.split(" ");
        int startX, startY, endX, endY;
        startX = Integer.parseInt(parts[parts.length - 3].split(",")[0]);
        startY = Integer.parseInt(parts[parts.length - 3].split(",")[1]);
        endX = Integer.parseInt(parts[parts.length - 1].split(",")[0]);
        endY = Integer.parseInt(parts[parts.length - 1].split(",")[1]);

        for (int x = startX; x <= endX; x++) {
            for (int y = startY; y <= endY; y++) {
                if (instruction.startsWith("turn on")) {
                    grid[x][y]++;
                } else if (instruction.startsWith("turn off")) {
                    if (grid[x][y] > 0) {
                        grid[x][y]--;
                    }
                } else if (instruction.startsWith("toggle")) {
                    grid[x][y] += 2;
                }
            }
        }
    }

    private static int totalBrightness(int[][] grid) {
        int brightness = 0;
        for (int[] row : grid) {
            for (int light : row) {
                brightness += light;
            }
        }
        return brightness;
    }
}
