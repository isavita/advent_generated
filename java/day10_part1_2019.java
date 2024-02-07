
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class Solution {

    public static void main(String[] args) {
        boolean[][] asteroids = readAsteroids("input.txt");
        int maxCount = findBestAsteroidLocation(asteroids);
        System.out.println(maxCount);
    }

    public static boolean[][] readAsteroids(String filename) {
        boolean[][] asteroids = null;
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            int rowCount = 0;
            while ((line = br.readLine()) != null) {
                if (asteroids == null) {
                    asteroids = new boolean[line.length()][line.length()];
                }
                for (int i = 0; i < line.length(); i++) {
                    asteroids[rowCount][i] = line.charAt(i) == '#';
                }
                rowCount++;
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return asteroids;
    }

    public static int findBestAsteroidLocation(boolean[][] asteroids) {
        int maxCount = 0;
        for (int y = 0; y < asteroids.length; y++) {
            for (int x = 0; x < asteroids[y].length; x++) {
                if (asteroids[y][x]) {
                    int count = countVisibleAsteroids(asteroids, x, y);
                    if (count > maxCount) {
                        maxCount = count;
                    }
                }
            }
        }
        return maxCount;
    }

    public static int countVisibleAsteroids(boolean[][] asteroids, int x, int y) {
        Map<Double, Boolean> angles = new HashMap<>();
        for (int otherY = 0; otherY < asteroids.length; otherY++) {
            for (int otherX = 0; otherX < asteroids[otherY].length; otherX++) {
                if (asteroids[otherY][otherX] && !(otherX == x && otherY == y)) {
                    double angle = Math.atan2(otherY - y, otherX - x);
                    angles.put(angle, true);
                }
            }
        }
        return angles.size();
    }
}
