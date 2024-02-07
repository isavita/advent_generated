
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Solution {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            String data = "";
            while (scanner.hasNextLine()) {
                data += scanner.nextLine() + "\n";
            }
            scanner.close();
            
            int[] parsedInput = parseInput(data);
            int depth = parsedInput[0];
            int[] target = {parsedInput[1], parsedInput[2]};
            
            int[][] cave = makeCaveSystem(depth, target);
            int riskLevel = calculateRiskLevel(cave, target);
            System.out.println("Total Risk Level: " + riskLevel);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static int[] parseInput(String data) {
        String[] lines = data.split("\n");
        int depth = Integer.parseInt(lines[0].split(" ")[1]);
        String coords = lines[1].split(" ")[1];
        String[] parts = coords.split(",");
        int x = Integer.parseInt(parts[0]);
        int y = Integer.parseInt(parts[1]);
        return new int[]{depth, x, y};
    }

    public static int[][] makeCaveSystem(int depth, int[] target) {
        int[][] cave = new int[target[1] + 1][target[0] + 1];
        for (int y = 0; y <= target[1]; y++) {
            for (int x = 0; x <= target[0]; x++) {
                int geologicIndex;
                if ((x == 0 && y == 0) || (x == target[0] && y == target[1])) {
                    geologicIndex = 0;
                } else if (y == 0) {
                    geologicIndex = x * 16807;
                } else if (x == 0) {
                    geologicIndex = y * 48271;
                } else {
                    geologicIndex = cave[y][x - 1] * cave[y - 1][x];
                }
                cave[y][x] = (geologicIndex + depth) % 20183;
            }
        }
        return cave;
    }

    public static int calculateRiskLevel(int[][] cave, int[] target) {
        int riskLevel = 0;
        for (int y = 0; y <= target[1]; y++) {
            for (int x = 0; x <= target[0]; x++) {
                riskLevel += cave[y][x] % 3;
            }
        }
        return riskLevel;
    }
}
