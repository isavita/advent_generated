
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.stream.Stream;

public class Solution {
    public static void main(String[] args) {
        try {
            Stream<String> lines = Files.lines(Paths.get("input.txt"));
            int serial = Integer.parseInt(lines.findFirst().get().trim());
            final int gridSize = 300;
            int[][] grid = new int[gridSize][gridSize];

            for (int y = 0; y < gridSize; y++) {
                for (int x = 0; x < gridSize; x++) {
                    int rackID = x + 11;
                    int powerLevel = rackID * (y + 1);
                    powerLevel += serial;
                    powerLevel *= rackID;
                    powerLevel = (powerLevel / 100) % 10;
                    powerLevel -= 5;
                    grid[y][x] = powerLevel;
                }
            }

            int maxPower = Integer.MIN_VALUE;
            int maxX = 0, maxY = 0;
            for (int y = 0; y < gridSize - 2; y++) {
                for (int x = 0; x < gridSize - 2; x++) {
                    int totalPower = 0;
                    for (int dy = 0; dy < 3; dy++) {
                        for (int dx = 0; dx < 3; dx++) {
                            totalPower += grid[y + dy][x + dx];
                        }
                    }
                    if (totalPower > maxPower) {
                        maxPower = totalPower;
                        maxX = x + 1;
                        maxY = y + 1;
                    }
                }
            }

            System.out.printf("%d,%d\n", maxX, maxY);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
