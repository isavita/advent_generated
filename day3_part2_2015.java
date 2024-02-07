
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

public class Solution {
    public static void main(String[] args) {
        try {
            String directions = new String(Files.readAllBytes(Paths.get("input.txt")));
            Map<String, Boolean> visitedHouses = new HashMap<>();
            int xSanta = 0, ySanta = 0;
            int xRobo = 0, yRobo = 0;
            boolean isSantaTurn = true;

            visitedHouses.put(xSanta + "," + ySanta, true);

            for (char dir : directions.toCharArray()) {
                int x, y;
                if (isSantaTurn) {
                    x = xSanta;
                    y = ySanta;
                } else {
                    x = xRobo;
                    y = yRobo;
                }

                switch (dir) {
                    case '^':
                        y++; // Move north
                        break;
                    case 'v':
                        y--; // Move south
                        break;
                    case '>':
                        x++; // Move east
                        break;
                    case '<':
                        x--; // Move west
                        break;
                }

                visitedHouses.put(x + "," + y, true);
                if (isSantaTurn) {
                    xSanta = x;
                    ySanta = y;
                } else {
                    xRobo = x;
                    yRobo = y;
                }

                isSantaTurn = !isSantaTurn;
            }

            System.out.println(visitedHouses.size());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
