import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class HydrothermalVenture {
    public static void main(String[] args) throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
            Map<String, Integer> pointCounts = new HashMap<>();
            String line;
            while ((line = reader.readLine()) != null) {
                String[] parts = line.split(" -> ");
                String[] startCoords = parts[0].split(",");
                String[] endCoords = parts[1].split(",");
                int startX = Integer.parseInt(startCoords[0]);
                int startY = Integer.parseInt(startCoords[1]);
                int endX = Integer.parseInt(endCoords[0]);
                int endY = Integer.parseInt(endCoords[1]);

                if (startX == endX) { // vertical line
                    int minY = Math.min(startY, endY);
                    int maxY = Math.max(startY, endY);
                    for (int y = minY; y <= maxY; y++) {
                        String point = startX + "," + y;
                        pointCounts.put(point, pointCounts.getOrDefault(point, 0) + 1);
                    }
                } else if (startY == endY) { // horizontal line
                    int minX = Math.min(startX, endX);
                    int maxX = Math.max(startX, endX);
                    for (int x = minX; x <= maxX; x++) {
                        String point = x + "," + startY;
                        pointCounts.put(point, pointCounts.getOrDefault(point, 0) + 1);
                    }
                }
            }

            int overlapCount = 0;
            for (int count : pointCounts.values()) {
                if (count >= 2) {
                    overlapCount++;
                }
            }

            System.out.println("Number of points with at least two overlapping lines: " + overlapCount);
        }
    }
}