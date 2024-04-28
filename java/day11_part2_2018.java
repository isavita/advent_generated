import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {
    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        int serial = Integer.parseInt(br.readLine());

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

        int[][] summedAreaTable = new int[gridSize + 1][gridSize + 1];
        for (int y = 0; y < gridSize; y++) {
            for (int x = 0; x < gridSize; x++) {
                summedAreaTable[y + 1][x + 1] = grid[y][x] + summedAreaTable[y][x + 1] + summedAreaTable[y + 1][x] - summedAreaTable[y][x];
            }
        }

        int maxPower = Integer.MIN_VALUE;
        int maxX = 0, maxY = 0, maxSize = 0;
        for (int size = 1; size <= gridSize; size++) {
            for (int y = 0; y < gridSize - size + 1; y++) {
                for (int x = 0; x < gridSize - size + 1; x++) {
                    int totalPower = summedAreaTable[y + size][x + size] - summedAreaTable[y][x + size] - summedAreaTable[y + size][x] + summedAreaTable[y][x];
                    if (totalPower > maxPower) {
                        maxPower = totalPower;
                        maxX = x + 1;
                        maxY = y + 1;
                        maxSize = size;
                    }
                }
            }
        }

        System.out.printf("%d,%d,%d\n", maxX, maxY, maxSize);
    }
}