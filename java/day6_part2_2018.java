
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String content = "";
            String line;
            while ((line = reader.readLine()) != null) {
                content += line + "\n";
            }
            reader.close();

            String[] lines = content.trim().split("\n");
            Coordinate[] coordinates = parseCoordinates(lines);

            int regionSize = findRegionSize(coordinates, 10000);

            System.out.println(regionSize);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static class Coordinate {
        int x, y;

        Coordinate(int x, int y) {
            this.x = x;
            this.y = y;
        }
    }

    static Coordinate[] parseCoordinates(String[] lines) {
        Coordinate[] coordinates = new Coordinate[lines.length];

        for (int i = 0; i < lines.length; i++) {
            String[] parts = lines[i].split(", ");
            coordinates[i] = new Coordinate(Integer.parseInt(parts[0]), Integer.parseInt(parts[1]));
        }

        return coordinates;
    }

    static int findRegionSize(Coordinate[] coordinates, int maxDistance) {
        BoundingBox boundingBox = findBoundingBox(coordinates);
        int regionSize = 0;

        for (int x = boundingBox.minX; x <= boundingBox.maxX; x++) {
            for (int y = boundingBox.minY; y <= boundingBox.maxY; y++) {
                int totalDistance = 0;

                for (Coordinate c : coordinates) {
                    totalDistance += manhattanDistance(x, y, c.x, c.y);
                }

                if (totalDistance < maxDistance) {
                    regionSize++;
                }
            }
        }

        return regionSize;
    }

    static class BoundingBox {
        int minX, minY, maxX, maxY;

        BoundingBox(int minX, int minY, int maxX, int maxY) {
            this.minX = minX;
            this.minY = minY;
            this.maxX = maxX;
            this.maxY = maxY;
        }
    }

    static BoundingBox findBoundingBox(Coordinate[] coordinates) {
        int minX = Integer.MAX_VALUE;
        int minY = Integer.MAX_VALUE;
        int maxX = Integer.MIN_VALUE;
        int maxY = Integer.MIN_VALUE;

        for (Coordinate c : coordinates) {
            if (c.x < minX) {
                minX = c.x;
            }
            if (c.y < minY) {
                minY = c.y;
            }
            if (c.x > maxX) {
                maxX = c.x;
            }
            if (c.y > maxY) {
                maxY = c.y;
            }
        }

        return new BoundingBox(minX, minY, maxX, maxY);
    }

    static int manhattanDistance(int x1, int y1, int x2, int y2) {
        return Math.abs(x1 - x2) + Math.abs(y1 - y2);
    }
}
