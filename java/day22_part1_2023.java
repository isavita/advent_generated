
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class SandSlabs {

    public static void main(String[] args) {
        try {
            List<Brick> bricks = readInput("input.txt");
            settleBricks(bricks);

            int safeToDisintegrate = 0;
            for (int i = 0; i < bricks.size(); i++) {
                if (isSafeToDisintegrate(bricks, i)) {
                    safeToDisintegrate++;
                }
            }

            System.out.println(safeToDisintegrate);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static List<Brick> readInput(String filename) throws IOException {
        List<Brick> bricks = new ArrayList<>();
        BufferedReader reader = new BufferedReader(new FileReader(filename));
        String line;
        while ((line = reader.readLine()) != null) {
            bricks.add(parseBrick(line));
        }
        reader.close();
        return bricks;
    }

    private static Brick parseBrick(String line) {
        String[] parts = line.split("~");
        String[] startCoords = parts[0].split(",");
        String[] endCoords = parts[1].split(",");
        return new Brick(
                Integer.parseInt(startCoords[0]),
                Integer.parseInt(startCoords[1]),
                Integer.parseInt(startCoords[2]),
                Integer.parseInt(endCoords[0]),
                Integer.parseInt(endCoords[1]),
                Integer.parseInt(endCoords[2])
        );
    }

    private static void settleBricks(List<Brick> bricks) {
        bricks.sort(Comparator.comparingInt(b -> b.z1));

        for (int i = 0; i < bricks.size(); i++) {
            Brick current = bricks.get(i);
            int maxZ = 1;
            for (int j = 0; j < i; j++) {
                Brick other = bricks.get(j);
                if (current.intersects(other)) {
                    maxZ = Math.max(maxZ, other.z2 + 1);
                }
            }
            current.z2 -= (current.z1 - maxZ);
            current.z1 = maxZ;
        }
        bricks.sort(Comparator.comparingInt(b -> b.z1));
    }

    private static boolean isSafeToDisintegrate(List<Brick> bricks, int index) {
        Brick disintegrated = bricks.get(index);
        for (int i = index + 1; i < bricks.size(); i++) {
            Brick above = bricks.get(i);
            if (above.z1 <= disintegrated.z2) continue;

            boolean supported = false;
            for (int j = 0; j < bricks.size(); j++) {
                if (i == j || j == index) continue;
                Brick other = bricks.get(j);
                if (above.z1 == other.z2 + 1 && above.intersects(other)) {
                    supported = true;
                    break;
                }
            }
            if (!supported) {
                return false;
            }
        }
        return true;
    }

    static class Brick {
        int x1, y1, z1, x2, y2, z2;

        public Brick(int x1, int y1, int z1, int x2, int y2, int z2) {
            this.x1 = x1;
            this.y1 = y1;
            this.z1 = z1;
            this.x2 = x2;
            this.y2 = y2;
            this.z2 = z2;
        }

        public boolean intersects(Brick other) {
            return Math.max(x1, other.x1) <= Math.min(x2, other.x2) &&
                   Math.max(y1, other.y1) <= Math.min(y2, other.y2);
        }
    }
}
